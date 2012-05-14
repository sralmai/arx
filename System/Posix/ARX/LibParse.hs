{-# LANGUAGE OverloadedStrings
  #-}
module System.Posix.ARX.LibParse where

import Data.Attoparsec.Char8


signature :: Parser [Either ByteString ByteString]
signature = do
  string "####"
  spaces
  many (eitherP var func)

