module System.Posix.ARX.URI where

import Network.URI


normalized :: URI -> String
normalized  = normalizeCase . normalizeEscape . normalizePathSegments
            . ($ "") . uriToString id

