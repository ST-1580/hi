module HW3.Evaluator (
    eval
    ) where

import HW3.Base (HiAction (..), HiError (..), HiExpr (..), HiFun (..), HiMonad (runAction), HiValue (..))
import Codec.Compression.Zlib (CompressParams (compressLevel), bestCompression, compressWith, decompressWith, defaultCompressParams, defaultDecompressParams)
import Codec.Serialise (deserialise, serialise)
import Control.Monad.Except (ExceptT, MonadError (throwError), MonadTrans (lift), runExceptT)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import Data.Map (Map, adjust, assocs, elems, keys)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Data.Sequence (Seq, empty, fromList, (><))
import Data.Text (Text, strip, toLower, toUpper)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time (addUTCTime, diffUTCTime)
import Data.Word (Word8)
import Text.Read (readMaybe)

import qualified Data.ByteString as DB
import qualified Data.Map as DM
import qualified Data.Sequence as DS
import qualified Data.Text as DT

type WithExceptT m = ExceptT HiError m

-- Returns integer part of Rational and checks fractional part
getIntPartOfRational :: Rational -> (Int, Bool)
getIntPartOfRational number = let upper = fromInteger (numerator number)
                                  down = denominator number
                              in (upper, down == 1)

-- Returns stimes values
stimesAny :: HiMonad m => Semigroup a => (a -> HiValue) -> a -> Rational -> WithExceptT m HiValue
stimesAny constr value times = let (intPart, isInt) = getIntPartOfRational times
                        in if isInt && 0 < intPart
                              then return (constr (stimes intPart value))
                              else throwError HiErrorInvalidArgument

-- Calculates function by accumulator and current value
functionToFoldL :: HiMonad m => HiFun -> WithExceptT m HiValue -> HiValue -> WithExceptT m HiValue
functionToFoldL func acc curr = do accValue <- acc
                                   evalHiValueFunc func [HiExprValue accValue, HiExprValue curr]

-- Fold realization
customFold :: HiMonad m => HiFun -> Seq HiValue -> WithExceptT m HiValue
customFold func list = case DS.lookup 0 list of
                                    Just x -> foldl (functionToFoldL func) (return x) (DS.drop 1 list)
                                    Nothing -> return HiValueNull

-- Checks that given number positive and less than 256
checkNumber :: HiMonad m => Rational -> WithExceptT m Integer
checkNumber number = let (intPart, isInt) = getIntPartOfRational number
                     in if isInt && (0 <= intPart && intPart <= 255)
                            then return (toInteger intPart)
                            else throwError HiErrorInvalidArgument

-- Checks that given list is correct for ByteString
checkListNumbersForBytes :: HiMonad m => [HiValue] -> WithExceptT m [Integer]
checkListNumbersForBytes [] = return []
checkListNumbersForBytes (arg : args) = do value <- case arg of
                                                HiValueNumber number -> checkNumber number
                                                _ -> throwError HiErrorInvalidArgument
                                           values <- checkListNumbersForBytes args
                                           return (value : values)

-- Helper fold function for counting
foldFunctionToCount :: Map HiValue Int -> HiValue -> Map HiValue Int
foldFunctionToCount resMap value = case DM.lookup value resMap of
                                        Nothing -> DM.insert value 1 resMap
                                        Just _  -> adjust (1+) value resMap
-- Converts list to count map
listToMap :: [HiValue] -> Map HiValue HiValue
listToMap values = DM.map (\cnt -> HiValueNumber (toRational cnt)) (foldl foldFunctionToCount DM.empty values)

-- Helper fold function for invert
foldFunctionToInvert :: Map HiValue [HiValue] -> (HiValue, HiValue) -> Map HiValue [HiValue]
foldFunctionToInvert resMap (key, value) = case DM.lookup value resMap of
                                                Nothing -> DM.insert value [key] resMap
                                                Just _  -> adjust (key :) value resMap
-- Inverts map
invertMap :: Map HiValue HiValue -> Map HiValue HiValue
invertMap values = DM.map (\resSeq -> HiValueList (fromList resSeq)) (foldl foldFunctionToInvert DM.empty (assocs values))

-- Converts Text to Seq HiValue
textToList :: Text -> Seq HiValue
textToList text = DS.fromList (map (\c -> HiValueString (DT.pack [c])) (DT.unpack text))

-- Converts ByteString to Seq HiValue
bytesToList :: ByteString -> Seq HiValue
bytesToList bytes = DS.fromList (map (\word -> HiValueNumber (toRational word)) (DB.unpack bytes))

compressBytes :: ByteString -> ByteString
compressBytes bytes = toStrict (compressWith defaultCompressParams {compressLevel = bestCompression} (fromStrict bytes))

-- Helper for decompress
decompressBytes :: ByteString -> ByteString
decompressBytes bytes = toStrict (decompressWith defaultDecompressParams (fromStrict bytes))

-- Returns HiFun arity
getHiFunArity :: HiFun -> Int
getHiFunArity HiFunList           = -1
getHiFunArity HiFunNot            = 1
getHiFunArity HiFunLength         = 1
getHiFunArity HiFunToUpper        = 1
getHiFunArity HiFunToLower        = 1
getHiFunArity HiFunReverse        = 1
getHiFunArity HiFunTrim           = 1
getHiFunArity HiFunPackBytes      = 1
getHiFunArity HiFunUnpackBytes    = 1
getHiFunArity HiFunEncodeUtf8     = 1
getHiFunArity HiFunDecodeUtf8     = 1
getHiFunArity HiFunZip            = 1
getHiFunArity HiFunUnzip          = 1
getHiFunArity HiFunSerialise      = 1
getHiFunArity HiFunDeserialise    = 1
getHiFunArity HiFunRead           = 1
getHiFunArity HiFunMkDir          = 1
getHiFunArity HiFunChDir          = 1
getHiFunArity HiFunParseTime      = 1
getHiFunArity HiFunEcho           = 1
getHiFunArity HiFunCount          = 1
getHiFunArity HiFunKeys           = 1
getHiFunArity HiFunValues         = 1
getHiFunArity HiFunInvert         = 1
getHiFunArity HiFunAdd            = 2
getHiFunArity HiFunSub            = 2
getHiFunArity HiFunMul            = 2
getHiFunArity HiFunDiv            = 2
getHiFunArity HiFunAnd            = 2
getHiFunArity HiFunOr             = 2
getHiFunArity HiFunLessThan       = 2
getHiFunArity HiFunGreaterThan    = 2
getHiFunArity HiFunEquals         = 2
getHiFunArity HiFunNotLessThan    = 2
getHiFunArity HiFunNotGreaterThan = 2
getHiFunArity HiFunNotEquals      = 2
getHiFunArity HiFunRange          = 2
getHiFunArity HiFunFold           = 2
getHiFunArity HiFunWrite          = 2
getHiFunArity HiFunRand           = 2
getHiFunArity HiFunIf             = 3

-- Evaluates all HiFun with possible argument
evalHiFun :: HiMonad m => HiFun -> [HiValue] -> WithExceptT m HiValue
evalHiFun HiFunAdd [HiValueNumber l, HiValueNumber r]         = return (HiValueNumber (l + r))
evalHiFun HiFunAdd [HiValueString l, HiValueString r]         = return (HiValueString (DT.concat [l, r]))
evalHiFun HiFunAdd [HiValueList l, HiValueList r]             = return (HiValueList (l >< r))
evalHiFun HiFunAdd [HiValueBytes l, HiValueBytes r]           = return (HiValueBytes (DB.append l r))
evalHiFun HiFunAdd [HiValueTime time, HiValueNumber shift]    = return (HiValueTime (addUTCTime (realToFrac shift) time))
evalHiFun HiFunAdd [HiValueNumber shift, HiValueTime time]    = return (HiValueTime (addUTCTime (realToFrac shift) time))
evalHiFun HiFunSub [HiValueNumber l, HiValueNumber r]         = return (HiValueNumber (l - r))
evalHiFun HiFunSub [HiValueTime l, HiValueTime r]             = return (HiValueNumber (toRational (diffUTCTime l r)))
evalHiFun HiFunMul [HiValueNumber l, HiValueNumber r]         = return (HiValueNumber (l * r))
evalHiFun HiFunMul [HiValueString text, HiValueNumber times]  = stimesAny HiValueString text times
evalHiFun HiFunMul [HiValueList list, HiValueNumber times]    = stimesAny HiValueList list times
evalHiFun HiFunMul [HiValueBytes bytes, HiValueNumber times]  = stimesAny HiValueBytes bytes times
evalHiFun HiFunDiv [HiValueString l, HiValueString r]         = return (HiValueString (DT.concat [l, DT.pack "/", r]))
evalHiFun HiFunDiv [HiValueNumber l, HiValueNumber r]         = if r == 0
                                                                  then throwError HiErrorDivideByZero
                                                                  else return (HiValueNumber (l / r))
evalHiFun HiFunNot [HiValueBool boolean]                      = return (HiValueBool (not boolean))
evalHiFun HiFunNotLessThan [l, r]                             = return (HiValueBool (l >= r))
evalHiFun HiFunNotGreaterThan [l, r]                          = return (HiValueBool (l <= r))
evalHiFun HiFunLessThan [l, r]                                = return (HiValueBool (l < r))
evalHiFun HiFunGreaterThan [l, r]                             = return (HiValueBool (l > r))
evalHiFun HiFunEquals [l, r]                                  = return (HiValueBool (l == r))
evalHiFun HiFunNotEquals [l, r]                               = return (HiValueBool (l /= r))
evalHiFun HiFunLength [HiValueString text]                    = return (HiValueNumber (toRational (DT.length text)))
evalHiFun HiFunLength [HiValueList list]                      = return (HiValueNumber (toRational (DS.length list)))
evalHiFun HiFunLength [HiValueBytes bytes]                    = return (HiValueNumber (toRational (DB.length bytes)))
evalHiFun HiFunToUpper [HiValueString text]                   = return (HiValueString (toUpper text))
evalHiFun HiFunToLower [HiValueString text]                   = return (HiValueString (toLower text))
evalHiFun HiFunReverse [HiValueString text]                   = return (HiValueString (DT.reverse text))
evalHiFun HiFunReverse [HiValueList list]                     = return (HiValueList (DS.reverse list))
evalHiFun HiFunReverse [HiValueBytes bytes]                   = return (HiValueBytes (DB.reverse bytes))
evalHiFun HiFunTrim [HiValueString text]                      = return (HiValueString (strip text))
evalHiFun HiFunList args                                      = return (HiValueList (fromList args))
evalHiFun HiFunRange [HiValueNumber from, HiValueNumber to]   = return (HiValueList (fromList (map HiValueNumber [from..to])))
evalHiFun HiFunFold [HiValueFunction func, HiValueList list]  = let arity = getHiFunArity func
                                                                in if DS.length list == 1
                                                                    then return (DS.index list 0)
                                                                    else if arity == 2
                                                                            then customFold func list
                                                                            else throwError HiErrorInvalidFunction
evalHiFun HiFunPackBytes [HiValueList list]                   = do rationalList <- checkListNumbersForBytes (toList list)
                                                                   return (HiValueBytes (DB.pack (map fromIntegral rationalList)))
evalHiFun HiFunUnpackBytes [HiValueBytes bytes]               = return (HiValueList (bytesToList bytes))
evalHiFun HiFunEncodeUtf8 [HiValueString text]                = return (HiValueBytes (encodeUtf8 text))
evalHiFun HiFunDecodeUtf8 [HiValueBytes bytes]                = case decodeUtf8' bytes of
                                                                    Left _     -> return HiValueNull
                                                                    Right text -> return (HiValueString text)
evalHiFun HiFunZip [HiValueBytes bytes]                       = return (HiValueBytes (compressBytes bytes))
evalHiFun HiFunUnzip [HiValueBytes bytes]                     = return (HiValueBytes (decompressBytes bytes))
evalHiFun HiFunSerialise [value]                              = return (HiValueBytes (toStrict (serialise value)))
evalHiFun HiFunDeserialise [HiValueBytes bytes]               = return (deserialise (fromStrict bytes))
evalHiFun HiFunRead [HiValueString text]                      = return (HiValueAction (HiActionRead (DT.unpack text)))
evalHiFun HiFunWrite [HiValueString path, HiValueString text] = return (HiValueAction (HiActionWrite (DT.unpack path) (encodeUtf8 text)))
evalHiFun HiFunWrite [HiValueString path, HiValueBytes bytes] = return (HiValueAction (HiActionWrite (DT.unpack path) bytes))
evalHiFun HiFunMkDir [HiValueString text]                     = return (HiValueAction (HiActionMkDir (DT.unpack text)))
evalHiFun HiFunChDir [HiValueString text]                     = return (HiValueAction (HiActionChDir (DT.unpack text)))
evalHiFun HiFunParseTime [HiValueString text]                 = case readMaybe (DT.unpack text) of
                                                                    Nothing   -> return HiValueNull
                                                                    Just time -> return (HiValueTime time)
evalHiFun HiFunRand [HiValueNumber from, HiValueNumber to]    = let (intTo, isIntTo) = getIntPartOfRational to
                                                                    (intFrom, isIntFrom) = getIntPartOfRational from
                                                                in if isIntTo && isIntFrom && (intFrom <= intTo)
                                                                        then return (HiValueAction (HiActionRand intFrom intTo))
                                                                        else throwError HiErrorInvalidArgument
evalHiFun HiFunEcho [HiValueString text]                      = return (HiValueAction (HiActionEcho text))
evalHiFun HiFunCount [HiValueString text]                     = return (HiValueDict (listToMap (toList (textToList text))))
evalHiFun HiFunCount [HiValueList list]                       = return (HiValueDict (listToMap (toList list)))
evalHiFun HiFunCount [HiValueBytes bytes]                     = return (HiValueDict (listToMap (toList (bytesToList bytes))))
evalHiFun HiFunInvert [HiValueDict dict]                      = return (HiValueDict (invertMap dict))
evalHiFun HiFunKeys [HiValueDict dict]                        = return (HiValueList (fromList (keys dict)))
evalHiFun HiFunValues [HiValueDict dict]                      = return (HiValueList (fromList (elems dict)))
evalHiFun _ _ = throwError HiErrorInvalidArgument

-- Evaluates HiFunAnd
evalAnd :: HiMonad m => [HiExpr] -> WithExceptT m HiValue
evalAnd [l, r] = do evalL <- evalHiExpr l
                    case evalL of
                        HiValueNull       -> return HiValueNull
                        HiValueBool False -> return (HiValueBool False)
                        _ -> evalHiExpr r
evalAnd _      = throwError HiErrorInvalidArgument

-- Evaluates HiFunOr
evalOr :: HiMonad m => [HiExpr] -> WithExceptT m HiValue
evalOr [l, r] = do evalL <- evalHiExpr l
                   case evalL of
                        HiValueBool False -> evalHiExpr r
                        HiValueNull       -> evalHiExpr r
                        value             -> return value
evalOr _      = throwError HiErrorInvalidArgument

-- Evaluates HiFunIf
evalIf :: HiMonad m => [HiExpr] -> WithExceptT m HiValue
evalIf [condition, ifTrue, ifFalse] = do evalCondition <- evalHiExpr condition
                                         case evalCondition of
                                            HiValueBool True  -> evalHiExpr ifTrue
                                            HiValueBool False -> evalHiExpr ifFalse
                                            _ -> throwError HiErrorInvalidArgument
evalIf _ = throwError HiErrorInvalidArgument

-- Evaluates any argument of function
evalAllArgs :: HiMonad m => [HiExpr] -> WithExceptT m [HiValue]
evalAllArgs []           = return []
evalAllArgs (arg : args) = do value <- evalHiExpr arg
                              values <- evalAllArgs args
                              return (value : values)

-- Evalutes any not lazy HiFun
evalArgs :: HiMonad m => HiFun -> [HiExpr] -> WithExceptT m HiValue
evalArgs func args = do values <- evalAllArgs args
                        evalHiFun func values

-- Evalutes any HiFun
evalHiValueFunc :: HiMonad m => HiFun -> [HiExpr] -> WithExceptT m HiValue
evalHiValueFunc HiFunIf args  = evalIf args
evalHiValueFunc HiFunAnd args = evalAnd args
evalHiValueFunc HiFunOr args  = evalOr args
evalHiValueFunc func args     = evalArgs func args

-- Evaluates list element by index
evalListElem :: HiMonad m => Seq HiValue -> HiValue -> WithExceptT m HiValue
evalListElem list (HiValueNumber index) = let (intPartIndex, isInt) = getIntPartOfRational index
                                          in if isInt
                                                then if 0 <= intPartIndex && intPartIndex < DS.length list
                                                        then evalSubList False list (intPartIndex, intPartIndex + 1)
                                                        else return HiValueNull
                                                else throwError HiErrorInvalidArgument
evalListElem _ _ = throwError HiErrorInvalidArgument

-- Returns first element of a list as a number
helperListElem :: HiMonad m => Seq HiValue -> HiValue -> WithExceptT m HiValue
helperListElem list value = do ansList <- evalListElem list value
                               case ansList of
                                   HiValueNull         -> return HiValueNull
                                   HiValueList oneElem -> case DS.lookup 0 oneElem of
                                                            Nothing  -> throwError HiErrorInvalidArgument
                                                            Just res -> return res
                                   _ -> throwError HiErrorInvalidArgument

-- Subs list len from index
newId :: (Int, Int) -> Int
newId (index, len) = if index < 0 then len + index else index

-- Evaluates sublist of a given list
evalSubList :: HiMonad m => Bool -> Seq HiValue -> (Int, Int) -> WithExceptT m HiValue
evalSubList canChange list (from, to) = let len = DS.length list
                                        in if from >= 0 && to >= 0
                                                then if from <= to
                                                        then return (HiValueList (DS.drop from (DS.take to list)))
                                                        else return (HiValueList empty)
                                                else if canChange
                                                        then evalSubList False list (newId (from, len), newId (to, len))
                                                        else return (HiValueList empty)
-- Checks that given indexes are correct
checkSliceIndexes :: HiMonad m => Seq HiValue -> (Rational, Rational) -> WithExceptT m HiValue
checkSliceIndexes list (from, to) = let (intPartTo, isIntTo) = getIntPartOfRational to
                                        (intPartFrom, isIntFrom) = getIntPartOfRational from
                                    in if isIntTo && isIntFrom
                                          then evalSubList True list (intPartFrom, intPartTo)
                                          else throwError HiErrorInvalidArgument
-- Helper for list slicing
helperListSlice:: HiMonad m => Seq HiValue -> (HiValue, HiValue) -> WithExceptT m HiValue
helperListSlice list (HiValueNumber from, HiValueNumber to) = checkSliceIndexes list (from, to)
helperListSlice list (HiValueNull, HiValueNumber to)        = checkSliceIndexes list (0, to)
helperListSlice list (HiValueNumber from, HiValueNull)      = checkSliceIndexes list (from, toRational (DS.length list))
helperListSlice list (HiValueNull, HiValueNull)             = checkSliceIndexes list (0, toRational (DS.length list))
helperListSlice _ _ = throwError HiErrorInvalidArgument

-- Converts List of HiValueString to String
listToString :: HiMonad m => [HiValue] -> WithExceptT m String
listToString = foldr (\curr acc -> case curr of
                                        HiValueString text -> do accStr <- acc
                                                                 return (DT.head text : accStr)
                                        _ -> throwError HiErrorInvalidArgument) (return "")

-- Converts List of HiValueBytes to [Word8]
listToBytes:: HiMonad m => [HiValue] -> WithExceptT m [Word8]
listToBytes = foldr (\curr acc -> case curr of
                                        HiValueBytes bytes -> do accBytes <- acc
                                                                 return (DB.head bytes : accBytes)
                                        _ -> throwError HiErrorInvalidArgument) (return [])

-- Packs converted results to given constructor by given packer
packSwapResults :: HiMonad m => HiValue -> ([HiValue] -> WithExceptT m [a]) -> ([a] -> b) -> (b -> HiValue) -> WithExceptT m HiValue
packSwapResults value converter packer constr = case value of
                                                    HiValueNull -> return HiValueNull
                                                    HiValueList resList -> do res <- converter (toList resList)
                                                                              return (constr (packer res))
                                                    _ -> throwError HiErrorInvalidArgument
-- Evaluates HiValueString indexing
evalTextChar :: HiMonad m => Text -> HiValue -> WithExceptT m HiValue
evalTextChar text val = do res <- evalListElem (textToList text) val
                           packSwapResults res listToString DT.pack HiValueString

-- Evaluates HiValueString slicing
evalSubstring :: HiMonad m => Text -> (HiValue, HiValue) -> WithExceptT m HiValue
evalSubstring text val = do res <- helperListSlice (textToList text) val
                            packSwapResults res listToString DT.pack HiValueString

-- Evaluates HiValueBytes indexing   
evalBytesElem :: HiMonad m => ByteString -> HiValue -> WithExceptT m HiValue
evalBytesElem bytes val = do res <- evalListElem (bytesToListOfBytes bytes) val
                             packed <- packSwapResults res listToBytes DB.pack HiValueBytes
                             helperBytesElem packed

-- Evaluates HiValueBytes slicing                           
evalBytesSlice :: HiMonad m => ByteString-> (HiValue, HiValue) -> WithExceptT m HiValue
evalBytesSlice bytes val = do res <- helperListSlice (bytesToListOfBytes bytes) val
                              packSwapResults res listToBytes DB.pack HiValueBytes

-- Converts ByteString to Seq HiValueBytes
bytesToListOfBytes :: ByteString -> Seq HiValue
bytesToListOfBytes bytes = DS.fromList (map (\c -> HiValueBytes (DB.pack [c])) (DB.unpack bytes))

-- Helper for ByteString indexing
helperBytesElem :: HiMonad m => HiValue -> WithExceptT m HiValue
helperBytesElem toConvert = case toConvert of
                                HiValueNull          -> return HiValueNull
                                HiValueBytes oneByte -> return (HiValueNumber (toRational (DB.head oneByte)))
                                _ -> throwError HiErrorInvalidArgument

-- Evaluates when HiValueString, HiValueList or HiValueBytes using as a function
valueAsFunc :: HiMonad m => a -> [HiExpr]
               -> (a -> HiValue -> WithExceptT m HiValue, a -> (HiValue, HiValue) -> WithExceptT m HiValue)
               -> WithExceptT m HiValue
valueAsFunc value [a] (funcOneArg, _)     = do calcA <- evalHiExpr a
                                               funcOneArg value calcA
valueAsFunc value [a, b] (_, funcTwoArgs) = do calcA <- evalHiExpr a
                                               calcB <- evalHiExpr b
                                               funcTwoArgs value (calcA, calcB)
valueAsFunc _ _ _ = throwError HiErrorArityMismatch

-- Evaluates HiExprDict
helperDict :: HiMonad m => [(HiExpr, HiExpr)] -> WithExceptT m HiValue
helperDict pairs = do evaluated <- evalPairs pairs
                      return (HiValueDict (DM.fromList evaluated))

-- Evaluates any key and value of given dictionary
evalPairs :: HiMonad m => [(HiExpr, HiExpr)] -> WithExceptT m [(HiValue, HiValue)]
evalPairs []                      = return []
evalPairs ((key, value) : others) = do evalKey <- evalHiExpr key
                                       evalValue <- evalHiExpr value
                                       evalOthers <- evalPairs others
                                       return ((evalKey, evalValue) : evalOthers)

-- Evaluates all possible functions using arguments
evalFunctions :: HiMonad m => HiExpr -> [HiExpr] -> WithExceptT m HiValue
evalFunctions (HiExprDict pairs) args                   = do evalDict <- helperDict pairs
                                                             evalFunctions (HiExprValue evalDict) args
evalFunctions (HiExprApply func argsIn) argsOut         = do res <- evalFunctions func argsIn
                                                             evalFunctions (HiExprValue res) argsOut
evalFunctions (HiExprValue (HiValueFunction func)) args = let arity = getHiFunArity func
                                                          in if arity == -1 || arity == Prelude.length args
                                                                then evalHiValueFunc func args
                                                                else throwError HiErrorArityMismatch
evalFunctions (HiExprValue (HiValueDict dict)) args     = if Prelude.length args == 1
                                                                then do evalKey <- evalHiExpr (head args)
                                                                        case DM.lookup evalKey dict of
                                                                                Nothing    -> return HiValueNull
                                                                                Just value -> return value
                                                                else throwError HiErrorArityMismatch
evalFunctions (HiExprValue (HiValueString text)) args   = valueAsFunc text args (evalTextChar, evalSubstring)
evalFunctions (HiExprValue (HiValueList list)) args     = valueAsFunc list args (helperListElem, helperListSlice)
evalFunctions (HiExprValue (HiValueBytes bytes)) args   = valueAsFunc bytes args (evalBytesElem, evalBytesSlice)
evalFunctions _ _ = throwError HiErrorInvalidFunction

-- Runs HiValueAction
tryToRunAction :: HiMonad m => HiValue -> WithExceptT m HiValue
tryToRunAction (HiValueAction action) = lift (runAction action)
tryToRunAction _ = throwError HiErrorInvalidArgument

-- Find lowerest action
prepareToRunAction :: HiMonad m => HiExpr -> WithExceptT m HiValue
prepareToRunAction (HiExprRun action)      = do actionRes <- prepareToRunAction action
                                                prepareToRunAction (HiExprValue actionRes)
prepareToRunAction (HiExprApply func args) = do value <- evalFunctions func args
                                                tryToRunAction value
prepareToRunAction (HiExprValue value)     = tryToRunAction value
prepareToRunAction _ = throwError HiErrorInvalidArgument

-- Evaluates any HiExpr
evalHiExpr :: HiMonad m => HiExpr -> WithExceptT m HiValue
evalHiExpr (HiExprValue value)     = return value
evalHiExpr (HiExprApply func args) = evalFunctions func args
evalHiExpr (HiExprRun action)      = prepareToRunAction action
evalHiExpr (HiExprDict pairs)      = helperDict pairs

-- Starts evaluating using ExceptT monad
evalWithExceptT:: HiMonad m => HiExpr -> WithExceptT m HiValue
evalWithExceptT = evalHiExpr

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval input = runExceptT (evalWithExceptT input)