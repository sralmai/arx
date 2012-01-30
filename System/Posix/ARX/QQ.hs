{-# LANGUAGE TemplateHaskell
  #-}
module System.Posix.ARX.QQ(sQQ, bQQ) where

import Data.ByteString.Char8
import Language.Haskell.TH
import Language.Haskell.TH.Quote
 

{-| Quasi-quote for plain 'String's.
 -}
sQQ                         ::  QuasiQuoter
sQQ                          =  QuasiQuoter { quoteExp = stringE }

{-| Quasi-quoter for 'ByteString's. 
 -}
bQQ                         ::  QuasiQuoter
bQQ = QuasiQuoter { quoteExp = convertStringToByteStringExp }


-- The below code is derived from file-embed

convertStringToByteStringExp :: String -> Q Exp
convertStringToByteStringExp s = do
    helper                  <-  [| stringToBs |]
    return $! AppE helper $! LitE $! StringL s

stringToBs                  ::  String -> ByteString
stringToBs                   =  pack

