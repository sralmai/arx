
module System.Posix.ARX where

import Data.ByteString (ByteString)


data ARX                     =  ARX { run      :: [ByteString]
                                      env      :: [(ByteString, ByteString)]
                                      dir      :: Dir,
                                      archives :: [(Archive, ByteStream)]    }
deriving instance Eq ARX
deriving instance Ord ARX
deriving instance Show ARX


data ByteStream              =  Literal ByteString | FromFile ByteString
deriving instance Eq ByteSource
deriving instance Ord ByteSource
deriving instance Show ByteSource

data Archive                 =  Tar | TGZ | TBZ
deriving instance Eq Archive
deriving instance Ord Archive
deriving instance Show Archive

data Dir                     =  Fresh | Path ByteString
deriving instance Eq Dir
deriving instance Ord Dir
deriving instance Show Dir

