{-# LANGUAGE RecordWildCards #-}

module Helper (
    check, 
    TestRes (..)
    ) where

import HW3.Action (HiPermission (..), HIO (runHIO, HIO), PermissionException (..))
import HW3.Base (HiError, HiMonad (runAction), HiValue (HiValueNull))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import Control.Exception (catch)
import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity (runIdentity, Identity)
import Test.Tasty.HUnit (Assertion)

data TestRes = 
    ParseError String
  | EvalError HiError
  | Ok String
  | Perm HiPermission deriving (Show)

instance Eq TestRes where
  ParseError _ == ParseError _ = True
  EvalError a == EvalError b = a == b
  Ok a == Ok b = a == b
  Perm a == Perm b = a == b
  _ == _ = False

instance HiMonad Identity where
  runAction = const (return HiValueNull)

evalHelper :: Either HiError HiValue -> TestRes
evalHelper (Left err) = EvalError err
evalHelper (Right res) = Ok (show (prettyValue res))

calc :: HiMonad m => (m (Either HiError HiValue) -> TestRes) -> String -> TestRes
calc unwrap s = case parse s of
                        Left err -> ParseError "error"
                        Right expr -> unwrap (eval expr)

check :: String -> TestRes
check = calc (evalHelper . runIdentity)
