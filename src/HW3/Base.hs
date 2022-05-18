{-# LANGUAGE DeriveGeneric #-}

module HW3.Base (
    HiFun (..),
    HiValue (..),
    HiExpr (..),
    HiError (..),
    HiAction (..),
    HiMonad (..),
    ) where

import Codec.Serialise (Serialise)
import Data.ByteString (ByteString)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

data HiFun = 
      HiFunDiv 
    | HiFunMul
    | HiFunAdd
    | HiFunSub
    | HiFunNot
    | HiFunAnd
    | HiFunOr
    | HiFunLessThan
    | HiFunGreaterThan
    | HiFunEquals
    | HiFunNotLessThan
    | HiFunNotGreaterThan
    | HiFunNotEquals
    | HiFunIf
    | HiFunLength
    | HiFunToUpper
    | HiFunToLower
    | HiFunReverse
    | HiFunTrim
    | HiFunList
    | HiFunRange
    | HiFunFold
    | HiFunPackBytes
    | HiFunUnpackBytes
    | HiFunEncodeUtf8
    | HiFunDecodeUtf8
    | HiFunZip
    | HiFunUnzip
    | HiFunSerialise
    | HiFunDeserialise
    | HiFunRead
    | HiFunWrite
    | HiFunMkDir
    | HiFunChDir
    | HiFunParseTime
    | HiFunRand
    | HiFunEcho
    | HiFunCount
    | HiFunKeys
    | HiFunValues
    | HiFunInvert deriving (Show, Ord, Eq, Generic)

instance Serialise HiFun      

data HiValue = 
      HiValueNull
    | HiValueBool Bool
    | HiValueNumber Rational
    | HiValueFunction HiFun
    | HiValueString Text
    | HiValueList (Seq HiValue)
    | HiValueBytes ByteString
    | HiValueAction HiAction
    | HiValueTime UTCTime
    | HiValueDict (Map HiValue HiValue) deriving (Show, Ord, Eq, Generic)

instance Serialise HiValue   

data HiExpr = 
      HiExprValue HiValue
    | HiExprApply HiExpr [HiExpr]
    | HiExprRun HiExpr  
    | HiExprDict [(HiExpr, HiExpr)] deriving (Show, Ord, Eq)

data HiError =
      HiErrorInvalidArgument
    | HiErrorInvalidFunction
    | HiErrorArityMismatch
    | HiErrorDivideByZero deriving (Show, Ord, Eq)

data HiAction =
    HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text deriving (Show, Ord, Eq, Generic)

instance Serialise HiAction

class Monad m => HiMonad m where
    runAction :: HiAction -> m HiValue
