module HW3.Parser (
    parse
    ) where

import HW3.Base (HiExpr (..), HiValue (..), HiFun (..), HiAction (..))
import Control.Applicative ( Alternative((<|>), many) )
import Control.Monad.Combinators.Expr (makeExprParser, Operator (InfixL, InfixR, InfixN))
import Data.ByteString (pack)
import Data.Char (digitToInt, isAlpha, isAlphaNum)
import Data.Text (pack)
import Data.Void (Void)
import Data.Word (Word8)
import Text.Megaparsec (Parsec, runParser, MonadParsec (try, eof, notFollowedBy), choice, manyTill, satisfy, sepBy1)
import Text.Megaparsec.Char (string, space, char, hexDigitChar)
import Text.Megaparsec.Char.Lexer (scientific)
import Text.Megaparsec.Error ( ParseErrorBundle )

import qualified Text.Megaparsec.Char.Lexer as L

type HiParser = Parsec Void String

-- Skips space after parser
lexeme :: HiParser a -> HiParser a
lexeme = L.lexeme space

-- Skips space after string
symbol :: String -> HiParser String
symbol = lexeme . string

-- Parses HiValueNumber
parseHiValueNumber :: HiParser HiValue 
parseHiValueNumber = do num <- L.signed space scientific
                        return (HiValueNumber (toRational num))

-- Parses HiValueBool
parseHiValueBool :: HiParser HiValue
parseHiValueBool = choice [ HiValueBool True <$ string "true",
                            HiValueBool False <$ string "false"]

-- Parses any HiFun
parseHiFun :: HiParser HiFun
parseHiFun = choice [ HiFunDiv <$ string "div",
                      HiFunMul <$ string "mul",
                      HiFunAdd <$ string "add",
                      HiFunSub <$ string "sub",
                      HiFunAnd <$ string "and",
                      HiFunOr <$ string "or",
                      HiFunLessThan <$ string "less-than",
                      HiFunGreaterThan <$ string "greater-than",
                      HiFunEquals <$ string "equals",
                      HiFunNotLessThan <$ string "not-less-than",
                      HiFunNotGreaterThan <$ string "not-greater-than",
                      HiFunNotEquals <$ string "not-equals",                      
                      HiFunNot <$ string "not",
                      HiFunIf <$ string "if",
                      HiFunLength <$ string "length",
                      HiFunToUpper <$ string "to-upper",
                      HiFunToLower <$ string "to-lower",
                      HiFunReverse <$ string "reverse",
                      HiFunTrim <$ string "trim",
                      HiFunList <$ string "list",
                      HiFunRange <$ string "range",
                      HiFunFold <$ string "fold",
                      HiFunPackBytes <$ string "pack-bytes",
                      HiFunUnpackBytes <$ string "unpack-bytes",
                      HiFunEncodeUtf8 <$ string "encode-utf8",
                      HiFunDecodeUtf8 <$ string "decode-utf8",
                      HiFunZip <$ string "zip",
                      HiFunUnzip <$ string "unzip",
                      HiFunSerialise <$ string "serialise",
                      HiFunDeserialise <$ string "deserialise",
                      HiFunRead <$ string "read",
                      HiFunWrite <$ string "write",
                      HiFunMkDir <$ string "mkdir",
                      HiFunChDir <$ string "cd",
                      HiFunParseTime <$ string "parse-time",
                      HiFunRand <$ string "rand",
                      HiFunEcho <$ string "echo",
                      HiFunCount <$ string "count",
                      HiFunKeys <$ string "keys",
                      HiFunValues <$ string "values",
                      HiFunInvert <$ string "invert"]

-- Parses HiValueFunction                          
parseHiValueFunction :: HiParser HiValue
parseHiValueFunction = do func <- parseHiFun
                          return (HiValueFunction func)

-- Parses cwd and now action
parseHiAction :: HiParser HiAction
parseHiAction = choice [ HiActionCwd <$ string "cwd",
                         HiActionNow <$ string "now" ]

-- Parses HiValueAction
parseHiValueAction :: HiParser HiValue
parseHiValueAction = do action <- parseHiAction
                        return (HiValueAction action)

-- Parses HiValueNull
parseHiValueNull :: HiParser HiValue
parseHiValueNull = do _ <- symbol "null"
                      return HiValueNull

-- Parses HiValueSting 
parseHiValueString :: HiParser HiValue
parseHiValueString = do _ <- char '"'
                        res <- manyTill L.charLiteral (char '"')
                        return (HiValueString (Data.Text.pack res))

-- Parses one byte
parseByte :: HiParser Word8 
parseByte = do first <- hexDigitChar
               second <- hexDigitChar
               return (fromIntegral (16 * digitToInt first + digitToInt second))

-- Parses HiValueBytes
parseHiValueBytes :: HiParser HiValue
parseHiValueBytes = do _ <- symbol "[#"
                       res <- lexeme (helperSepBy " " parseByte) <|> return []
                       _ <- symbol "#]"
                       return (HiValueBytes (Data.ByteString.pack res))

-- Parses values separated by given string by giving parser
helperSepBy :: String -> HiParser a -> HiParser [a] 
helperSepBy separator nextParser = do firstByte <- nextParser
                                      bytes <- many (try (do _ <- symbol separator
                                                             nextParser))
                                      return (firstByte : bytes)                       

-- Parses any HiValue besides HiValueList
parseAnyValueBesidesList :: HiParser HiValue
parseAnyValueBesidesList = try parseHiValueFunction <|> try parseHiValueNumber <|> try parseHiValueBool
                           <|> try parseHiValueNull <|> try parseHiValueString <|> try parseHiValueBytes
                           <|> parseHiValueAction

-- Parses any HiValue besides HiValueList
parseHiExprValue :: HiParser HiExpr
parseHiExprValue = do value <- lexeme parseAnyValueBesidesList
                      return (HiExprValue value)

-- Parses function arguments inside brackets
parseArgs :: HiParser [HiExpr]
parseArgs = helperSepBy "," (lexeme startMakeExprParser)

-- Parses function arguments
parseFunctionArgs :: HiParser [HiExpr]
parseFunctionArgs = do _ <- symbol "("
                       args <- parseArgs <|> return []
                       _ <- symbol ")"
                       return args
-- Parses HiList              
parseHiExprList :: HiParser HiExpr
parseHiExprList = do _ <- symbol "["
                     args <- parseArgs <|> return []
                     _ <- symbol "]"
                     return (HiExprApply (HiExprValue (HiValueFunction HiFunList)) args)

-- Parses HiExprApply
parseHiExprApply :: HiExpr -> HiParser HiExpr
parseHiExprApply func = do args <- lexeme parseFunctionArgs
                           let res = HiExprApply func args
                           try (parseExpressionAccess res) <|> return res 

-- Parses HiExprAction
parseHiExprAction :: HiExpr -> HiParser HiExpr
parseHiExprAction expr = do _ <- symbol "!" 
                            parseExpressionAccess (HiExprRun expr) <|> return (HiExprRun expr)

-- Parses key and value of a dictionary
parseKeyValue :: HiParser (HiExpr, HiExpr)
parseKeyValue = do first <- lexeme startMakeExprParser
                   _ <- symbol ":"
                   second <- lexeme startMakeExprParser
                   return (first, second)

-- Parses HiExprDict
parseDictionary :: HiParser HiExpr
parseDictionary = do _ <- symbol "{"
                     res <- helperSepBy "," parseKeyValue <|> return []
                     _ <- symbol "}"
                     return (HiExprDict res)

-- Parses low lvl HiExpr
parseExpressions :: HiParser HiExpr
parseExpressions = do _ <- space
                      value <- try parseHiExprValue <|> try parseHiExprList <|> try parseDictionary
                                  <|> parseInsideBrackets startMakeExprParser   
                      try (parseExpressionAccess value) <|> return value

-- Parses dot access
parseDot :: HiExpr -> HiParser HiExpr
parseDot expr = do _ <- char '.'
                   word <- ((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy1` char '-'
                   let res = HiExprApply expr
                          [HiExprValue (HiValueString (Data.Text.pack (head word ++ foldl (\acc curr -> acc ++ "-" ++ curr) "" (tail word))))]
                   try (parseExpressionAccess res) <|> return res             

-- Parses HiExpr like E -> E(E, ... E) | E! | E.fid | (E)
parseHiExpr :: HiParser HiExpr
parseHiExpr = do x <- lexeme parseExpressions 
                 lexeme (parseExpressionAccess x) <|> return x

-- Parses (E, ... E) | E! | E.fid
parseExpressionAccess :: HiExpr ->  HiParser HiExpr
parseExpressionAccess expr = do try (parseHiExprApply expr) <|> try (parseHiExprAction expr) <|> parseDot expr

-- Starts parsing using makeExprParser
startMakeExprParser :: HiParser HiExpr 
startMakeExprParser = makeExprParser parserTerms parserOperatorTable 

-- Parses expression inside brackets by given parses
parseInsideBrackets :: HiParser HiExpr -> HiParser HiExpr
parseInsideBrackets parser = do _ <- space
                                _ <- symbol "("
                                res <- parser
                                _ <- symbol ")"
                                return res

parserTerms :: HiParser HiExpr
parserTerms = choice [ parseHiExpr,
                       parseInsideBrackets startMakeExprParser,
                       parseHiExprValue]

parserOperatorTable :: [[Operator HiParser HiExpr]]
parserOperatorTable = [[ binaryL "*" HiFunMul,
                         binaryLSamePrefix "/" HiFunDiv],
                       [ binaryL "+" HiFunAdd,
                         binaryL "-" HiFunSub],
                       [ binaryN "<=" HiFunNotGreaterThan,
                         binaryN ">=" HiFunNotLessThan,
                         binaryN "<" HiFunLessThan,
                         binaryN ">" HiFunGreaterThan],
                       [ binaryN "==" HiFunEquals,
                         binaryN "/=" HiFunNotEquals],
                       [ binaryR "&&" HiFunAnd],
                       [ binaryR "||" HiFunOr]]

-- Helper for create HiExprApply
pushLeftAndRight :: HiFun -> HiExpr -> HiExpr -> HiExpr
pushLeftAndRight func l r = HiExprApply (HiExprValue (HiValueFunction func)) [l, r] 

-- Parses binary operation with same prefix
binaryLSamePrefix :: String -> HiFun -> Operator HiParser HiExpr
binaryLSamePrefix name func = InfixL (pushLeftAndRight func <$ try (string name <* notFollowedBy (char '=')))

binaryL :: String -> HiFun -> Operator HiParser HiExpr
binaryL name func = InfixL (pushLeftAndRight func <$ string name)

binaryR :: String -> HiFun -> Operator HiParser HiExpr
binaryR name func = InfixR (pushLeftAndRight func <$ string name)

binaryN :: String -> HiFun -> Operator HiParser HiExpr
binaryN name func = InfixN (pushLeftAndRight func <$ string name)

-- Starts parsing
startParse :: HiParser HiExpr
startParse = do _ <- space
                res <- startMakeExprParser
                _ <- eof
                return res

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser startParse ""