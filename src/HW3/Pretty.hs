module HW3.Pretty (
    prettyValue
    ) where

import Data.ByteString (ByteString, unpack)
import Data.Foldable (toList)
import Data.Map (assocs)
import Data.Ratio (denominator, numerator)
import Data.Scientific (FPFormat (Fixed), formatScientific, fromRationalRepetendUnlimited)
import HW3.Base (HiAction (..), HiFun (..), HiValue (..))
import Numeric (showHex)
import Prettyprinter (Doc, Pretty (pretty), viaShow, (<+>))
import Prettyprinter.Render.Terminal (AnsiStyle)

-- Prints fraction up/down
prettyFraction :: Integer -> Integer -> Doc AnsiStyle
prettyFraction up down = pretty (show up) <> pretty "/" <> pretty (show down)

-- Prints Word8
prettyWord8 :: Integer -> Doc AnsiStyle
prettyWord8 word = if word < 16
                      then pretty "0" <> pretty (showHex word "")
                      else pretty (showHex word "")

-- Prints HiValueBytes
prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes bytes = pretty "[#"
                    <> foldl (\acc curr -> acc <+> prettyWord8 (toInteger curr)) (pretty "") (unpack bytes)
                    <+> pretty "#]"

-- Prints HiValueList
prettyHiList :: [HiValue] -> Doc AnsiStyle
prettyHiList []   = pretty "[" <+> pretty "]"
prettyHiList list = pretty "["
                    <+> foldl (\acc curr -> acc <> pretty "," <+> prettyValue curr) (prettyValue (head list)) (drop 1 list)
                    <+> pretty "]"

-- Prints key and value
prettyPair :: (HiValue, HiValue) -> Doc AnsiStyle
prettyPair (key, value) = prettyValue key <> pretty ":" <+> prettyValue value

-- Prints HiValueDict
prettyDict :: [(HiValue, HiValue)] -> Doc AnsiStyle
prettyDict []   = pretty "{" <+> pretty "}"
prettyDict dict = pretty "{"
                  <+> foldl (\acc pair -> acc <> pretty "," <+> prettyPair pair) (prettyPair (head dict)) (drop 1 dict)
                  <+> pretty "}"

-- Prints file path
prettyPath :: FilePath -> Doc AnsiStyle
prettyPath path = pretty "\"" <> pretty path <> pretty "\""

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue HiValueNull            = pretty "null"
prettyValue (HiValueString text)   = viaShow text
prettyValue (HiValueBytes bytes)   = prettyBytes bytes
prettyValue (HiValueList list)     = prettyHiList (toList list)
prettyValue (HiValueDict dict)     = prettyDict (assocs dict)
prettyValue (HiValueTime time)     = pretty "parse-time(\"" <> pretty (show time) <> pretty "\")"
prettyValue (HiValueBool boolean)  = if boolean then pretty "true" else pretty "false"
prettyValue (HiValueNumber number) = let (sc, rep) = fromRationalRepetendUnlimited number
                                         up = numerator number
                                         down = denominator number
                                         (intPart, fracPart) = quotRem up down
                                     in case rep of
                                         Nothing -> if down == 1
                                                        then pretty (show up)
                                                        else pretty (formatScientific Fixed Nothing sc)
                                         Just _  -> if intPart == 0
                                                        then prettyFraction fracPart down
                                                        else if fracPart < 0
                                                                then pretty (show intPart) <+> pretty "-"
                                                                        <+> prettyFraction (-1 * fracPart) down
                                                                else pretty (show intPart) <+> pretty "+"
                                                                        <+> prettyFraction fracPart down
prettyValue (HiValueAction action) = case action of
                                        HiActionCwd              -> pretty "cwd"
                                        HiActionNow              -> pretty "now"
                                        HiActionEcho text        -> pretty "echo(" <> viaShow text <> pretty ")"
                                        HiActionRead path        -> pretty "read(" <> prettyPath path <> pretty ")"
                                        HiActionMkDir path       -> pretty "mkdir(" <> prettyPath path <> pretty ")"
                                        HiActionChDir path       -> pretty "cd(" <> prettyPath path <> pretty ")"
                                        HiActionWrite path bytes -> pretty "write(" <> prettyPath path <> pretty ","
                                                                    <+> prettyBytes bytes <> pretty ")"
                                        HiActionRand from to     -> pretty "rand(" <> pretty (show from) <> pretty ","
                                                                    <+> pretty (show to) <> pretty ")"
prettyValue (HiValueFunction func) = case func of
                                        HiFunDiv            -> pretty "div"
                                        HiFunMul            -> pretty "mul"
                                        HiFunAdd            -> pretty "add"
                                        HiFunSub            -> pretty "sub"
                                        HiFunNot            -> pretty "not"
                                        HiFunAnd            -> pretty "and"
                                        HiFunOr             -> pretty "or"
                                        HiFunLessThan       -> pretty "less-than"
                                        HiFunGreaterThan    -> pretty "greater-than"
                                        HiFunEquals         -> pretty "equals"
                                        HiFunNotLessThan    -> pretty "not-less-than"
                                        HiFunNotGreaterThan -> pretty "not-greater-than"
                                        HiFunNotEquals      -> pretty "not-equals"
                                        HiFunIf             -> pretty "if"
                                        HiFunLength         -> pretty "length"
                                        HiFunToUpper        -> pretty "to-upper"
                                        HiFunToLower        -> pretty "to-lower"
                                        HiFunReverse        -> pretty "reverse"
                                        HiFunTrim           -> pretty "trim"
                                        HiFunList           -> pretty "list"
                                        HiFunRange          -> pretty "range"
                                        HiFunFold           -> pretty "fold"
                                        HiFunPackBytes      -> pretty "pack-bytes"
                                        HiFunUnpackBytes    -> pretty "unpack-bytes"
                                        HiFunEncodeUtf8     -> pretty "encode-utf8"
                                        HiFunDecodeUtf8     -> pretty "decode-utf8"
                                        HiFunZip            -> pretty "zip"
                                        HiFunUnzip          -> pretty "unzip"
                                        HiFunSerialise      -> pretty "serialise"
                                        HiFunDeserialise    -> pretty "deserialise"
                                        HiFunRead           -> pretty "read"
                                        HiFunWrite          -> pretty "write"
                                        HiFunMkDir          -> pretty "mkdir"
                                        HiFunChDir          -> pretty "cd"
                                        HiFunParseTime      -> pretty "parse-time"
                                        HiFunRand           -> pretty "rand"
                                        HiFunEcho           -> pretty "echo"
                                        HiFunCount          -> pretty "count"
                                        HiFunKeys           -> pretty "keys"
                                        HiFunValues         -> pretty "values"
                                        HiFunInvert         -> pretty "invert"
