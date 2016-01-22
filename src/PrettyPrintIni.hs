module PrettyPrintIni
    ( prettyPrint
    ) where

import ParseIni

-- Data.ByteString.Char8 and Data.ByteString both export the same (strict) ByteString type
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as M

-- |Pretty-print an @INIFile@ in INI format.
prettyPrint :: INIFile -> BC.ByteString
prettyPrint = M.foldrWithKey printEntry BC.empty
  where printEntry name sect stream = BC.append stream new_entry
          where new_entry = BC.append (printSectName name)
                                     (printSection sect)

spaces :: Int -> BC.ByteString
spaces n = BC.replicate n ' '

quote, eol, lbrac, rbrac, eq :: BC.ByteString
quote = BC.singleton '\"'
eol = BC.singleton '\n'
lbrac = BC.singleton '['
rbrac = BC.singleton ']'
eq = BC.singleton '='

printSectName :: INISectName -> BC.ByteString
printSectName (ISect s) = BC.concat [lbrac, s, rbrac, eol]
printSectName (ISubsect s1 s2) = BC.concat [
  lbrac, s1, spaces 1, quote, s2, quote, rbrac, eol
  ]

printSection :: INISection -> BC.ByteString
printSection = M.foldrWithKey pEntry BC.empty
  where
    pEntry key vals stream = BC.append stream (pKeyVal key vals)
    pKeyVal key vals = Prelude.foldr go BC.empty vals
      where
        go val prev = BC.append prev (BC.concat [
          spaces 4, key, spaces 1, eq, spaces 1, printValue val, eol
          ])

printValue :: INIVal -> BC.ByteString
printValue (IBool bool) = BC.pack (show bool)
printValue (IInt int) = BC.pack (show int)
-- Convert escaped value back!
printValue (IString str) = BC.concat [quote, str, quote]

