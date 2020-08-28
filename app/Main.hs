module Main where

import Lib
import Language.CSharp.Parser (parser)
import Language.CSharp.Lexer (lexer)

main :: IO ()
main = 
    someFunc
    -- parseCSharpCode

parseCSharpCode :: IO ()
parseCSharpCode = do 
    let filename = "sample.cs"
    code <- readFile filename
    print $ parser filename $ lexer code