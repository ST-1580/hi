module Main (
    main
    ) where
        
import HW3.Action (HIO (runHIO), HiPermission (..))
import HW3.Evaluator (eval)
import HW3.Parser (parse)
import HW3.Pretty (prettyValue)
import Control.Monad.IO.Class (liftIO)
import Data.Set (fromList)
import System.Console.Haskeline (runInputT, defaultSettings, getInputLine, outputStrLn, InputT)
import Text.Megaparsec.Error (errorBundlePretty)

main :: IO ()
main = runInputT defaultSettings loop
   where
       loop :: InputT IO ()
       loop = let permissions = fromList [AllowRead, AllowWrite, AllowTime]
              in do minput <- getInputLine "hi> "
                    case minput of
                        Nothing -> return ()
                        Just "quit" -> return ()
                        Just input -> do case parse input of
                                            Left parseError -> outputStrLn (errorBundlePretty parseError)
                                            Right expr -> do evaluatedExpr <- liftIO (runHIO (eval expr) permissions)
                                                             case evaluatedExpr of
                                                                Left evalError -> outputStrLn (show evalError)
                                                                Right value -> outputStrLn (show (prettyValue value))
                                         loop