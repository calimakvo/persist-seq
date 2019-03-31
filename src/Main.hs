{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Main where

import Data.Csv
import Data.Text
import Control.Monad
import Control.Monad.Reader
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as BL
import Control.Monad.Trans.Resource (runResourceT, ResourceT)
import Control.Monad.Logger (runNoLoggingT, NoLoggingT)
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sql
import Database.Persist.Postgresql
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
NiceGuy
    niceGuyId Int sqltype=bigint default=nextval('nice_guy_id_seq')
    name Text
    age Int
    authorizedDate Day Maybe sqltype=date
    regTime UTCTime sqltype=timestamptz
    Primary niceGuyId
    deriving Show
NiceGuyPet
    name Text
    niceGuyId NiceGuyId
    deriving Show
|]

getConnection :: ConnectionString
getConnection = "host=localhost port=5432 user=sample dbname=sampledb password="

runSQLAction :: SqlPersistT (ResourceT (NoLoggingT IO)) a -> IO a
runSQLAction = runNoLoggingT . runResourceT . withPostgresqlConn getConnection . runSqlConn

importNiceGuys :: IO ()
importNiceGuys = do
    csvData <- BL.readFile "guys.csv"
    case decode NoHeader csvData of
        Left err -> error err
        Right v -> V.forM_ v $ \(name, age :: Int, pet) -> insertNiceGuys (name, age, pet)

getNiceGuySeq :: MonadIO m => ReaderT SqlBackend m [Single Int]
getNiceGuySeq = rawSql "select nextval('nice_guy_id_seq')" []

insertNiceGuys :: (Text, Int, Text) -> IO ()
insertNiceGuys (name, age, pet) = runSQLAction $ do
    now <- liftIO $ getZonedTime >>= return . zonedTimeToUTC
    [Single seq] <- getNiceGuySeq
    nid <- insert $ NiceGuy {
                        niceGuyNiceGuyId = seq,
                        niceGuyName = name,
                        niceGuyAge = age,
                        niceGuyAuthorizedDate = Just $ utctDay now,
                        niceGuyRegTime = now
                    }
    pid <- insert $ NiceGuyPet {
                        niceGuyPetName = pet,
                        niceGuyPetNiceGuyId = nid
                    }
    liftIO $ putStrLn $ "insert nice_guy__id: " ++ show(nid) ++ " / nice_guy_pet_id: " ++ show(pid)

main :: IO ()
main = do
    runSQLAction $ runMigration migrateAll
    importNiceGuys
