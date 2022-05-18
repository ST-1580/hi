{-# LANGUAGE DeriveFunctor #-}

module HW3.Action (
    HiPermission (..),
    PermissionException (..),
    HIO (..)
    ) where

import HW3.Base (HiAction (..), HiMonad (runAction), HiValue (..))
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import Control.Monad (ap)
import Data.Sequence (fromList)
import Data.Set (Set, member)
import Data.Text (pack)
import Data.Text.Encoding (decodeUtf8')
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import System.Directory (createDirectory, doesFileExist, getCurrentDirectory, listDirectory, setCurrentDirectory)
import System.Random (getStdRandom, uniformR)

import qualified Data.ByteString as DB
import qualified Data.Text as DT

data HiPermission =
    AllowRead
  | AllowWrite
  | AllowTime deriving (Show, Eq, Ord)

data PermissionException = PermissionRequired HiPermission deriving (Show)

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a } deriving (Functor)

instance Applicative HIO where
    pure x = HIO (\_ -> pure x)
    p <*> q = ap p q

instance Monad HIO where
    m >>= f = joinHIO (fmap f m)

joinHIO :: HIO (HIO a) -> HIO a
joinHIO (HIO x1) = HIO (\prem -> do inside <- x1 prem
                                    case inside of
                                       HIO x2 -> x2 prem)

instance HiMonad HIO where
    runAction (HiActionRead path)       = HIO (\p -> if member AllowRead p
                                                        then do isFile <- doesFileExist path
                                                                helperRead isFile path
                                                        else throwIO (PermissionRequired AllowRead))
    runAction (HiActionWrite path text) = HIO (\p -> if member AllowWrite p
                                                        then do DB.writeFile path text
                                                                return HiValueNull
                                                        else throwIO (PermissionRequired AllowWrite))
    runAction (HiActionMkDir path)      = HIO (\p -> if member AllowWrite p
                                                        then do createDirectory path
                                                                return HiValueNull
                                                        else throwIO (PermissionRequired AllowWrite))
    runAction (HiActionChDir path)      = HIO (\p -> if member AllowRead p
                                                        then do setCurrentDirectory path
                                                                return HiValueNull
                                                        else throwIO (PermissionRequired AllowRead))
    runAction HiActionCwd               = HIO (\p -> if member AllowRead p
                                                        then do currDir <- getCurrentDirectory
                                                                return (HiValueString (pack currDir))
                                                        else throwIO (PermissionRequired AllowRead))
    runAction HiActionNow               = HIO (\p -> if member AllowTime p
                                                        then do threadDelay 25
                                                                sysTime <- getSystemTime
                                                                return (HiValueTime (systemToUTCTime sysTime))
                                                        else throwIO (PermissionRequired AllowTime))
    runAction (HiActionEcho text)       = HIO (\p -> if member AllowWrite p
                                                        then do putStrLn (DT.unpack text)
                                                                return HiValueNull
                                                        else throwIO (PermissionRequired AllowWrite))                                                       
    runAction (HiActionRand from to)    = HIO (\_ -> do res <- getStdRandom (uniformR (from, to))
                                                        return (HiValueNumber (toRational res)))

-- Helper for HiActionRead
helperRead :: Bool -> FilePath -> IO HiValue
helperRead isFile path = if not isFile
                            then do dirs <- listDirectory path
                                    return (HiValueList (fromList (map (\dir -> (HiValueString (pack dir))) dirs)))
                            else do file <- DB.readFile path
                                    case decodeUtf8' file of
                                        Left _     -> return (HiValueBytes file)
                                        Right text -> return (HiValueString text)