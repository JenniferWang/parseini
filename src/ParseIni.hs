module ParseIni
    ( INISectName (..)
    , INIKey
    , INIVal (..)
    , INISection
    , INIFile
    , parseIniFile
    , toSectName
    , toKey
    , lookupSection
    , lookupValue
    , main_test
    ) where

import Prelude hiding (takeWhile, take)
import Data.ByteString.Char8 (pack, unpack)
import Data.Attoparsec.ByteString.Char8(char, peekChar, space, anyChar)
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.Attoparsec.ByteString
import Control.Applicative
import Data.Char (toLower)


-- **** TYPES ****
-- These are the types you should use for the results of your parse.
-- Think carefully about what's going on here!

-- |INI files are separated into sections and subsections of key-value pairs.
-- We represent section and subsection identifiers with the INISectName type.
-- Section names are case insensitive strings; subsection names are case sensitive.
data INISectName = ISect    { iSect    :: B.ByteString }
                 | ISubsect { iSect    :: B.ByteString
                            , iSubsect :: B.ByteString }
    deriving (Eq, Ord, Show)

-- |Within each (sub)section, an INI file contains a set of keys and values.
-- Keys are case insensitive strings.
type INIKey = B.ByteString

-- |After parsing key-value pairs, each value should be assigned a type.
-- We represent these types via the @INIVal@ sum type.
data INIVal = IBool Bool
            | IInt Integer
            | IString B.ByteString
    deriving (Eq, Ord, Show)

-- |An @INISection@ is a map from @INIKey@s to @INIVal@s.
type INISection = M.Map INIKey [INIVal]

-- |An @INIFile@ is a map from @INISectName@s to @INISection@s.
type INIFile = M.Map INISectName INISection


-- **** INTERFACE ****
-- You need to implement these so that we can test your code!
--
-- Why? Because you shouldn't need to expose exactly the way that
-- you handle, e.g., case insensitive string matching in order for
-- someone to use your INI file parser.

-- |Given a section name and possibly a subsection name, return an
-- appropriate @INISectName@. This function accounts for the case
-- insensitivity of the section name.
-- TODO: How to convert?
toSectName :: String -> Maybe String -> INISectName
toSectName name Nothing = (ISect . pack) $ map toLower name
toSectName name (Just subsect) =
  ISubsect (pack $ map toLower name)
           (pack subsect)

-- |Given a key name, return an appropriate @INIKey@. This function
-- accounts for the case insensitivity of the key name.
toKey :: String -> INIKey
toKey key = pack $ map toLower key

-- |Look up a section in an @INIFile@.
lookupSection :: INISectName -> INIFile -> Maybe INISection
lookupSection = M.lookup

-- |Look up a value in an @INISection@.
lookupSValue :: INIKey -> INISection -> Maybe [INIVal]
lookupSValue = M.lookup

-- |Look up a value in an @INIFile@.
lookupValue :: INIKey -> INISectName -> INIFile -> Maybe [INIVal]
lookupValue key name file = case lookupSection name file of
                              Nothing   -> Nothing
                              Just sect -> lookupSValue key sect


-- **** PARSER ****

-- |Parse an INI file into an @INIFile@.
--
-- An INI file comprises a sequence of sections.
--
-- A section starts with a header, which declares the name of the section or subsection.
-- The header is followed by a sequence of key-value declarations.
--
-- Whitespace between and inside sections is ignored, as are comment lines, which
-- begin with @#@ or @;@.
parseIniFile :: B.ByteString -> Either String INIFile
-- parseIniFile = const $ Right M.empty
parseIniFile = parseOnly pINIFile

-- Your implementation goes here.
--
-- parseIniFile should return @Left errmsg@ on error,
-- or @Right parsedResult@ on success.

p_strToByte :: String -> Parser B.ByteString
p_strToByte = string . pack

isEOL = inClass "\n\b"

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close a = open *> a <* close

pSkipLine = skip (\w -> not $ isEOL w)

pLine = takeWhile (\w -> not $ isEOL w)

pSpaces = many space

anyByteString :: Parser B.ByteString
anyByteString = takeWhile (\_ -> True)

anyString :: Parser String
anyString = unpack <$> anyByteString

-- Parse comment
-- return ()
pComment = (p_strToByte "#" <|> p_strToByte ";") *> pSkipLine

pSectName :: Parser INISectName
pSectName = do
  names <- between (char '[') (char ']') pNames
  return $ toSectName (fst names) (snd names)

pNames :: Parser (String, Maybe String)
pNames = do
  name1 <- pName
  name2 <- between (pSpaces *> char '\"')
                   (char '\"' <* pSpaces)
                   pSubName
           <|> (return "")
  return $ case name2 of "" -> (name1, Nothing)
                         _  -> (name1, Just name2)

pName :: Parser String
pName = pSpaces *> many (C.satisfy (C.inClass "a-zA-Z0-9-."))

pSubName :: Parser String
pSubName = many namechar
  where namechar = (char '\\' *> pEscape)
                  <|> C.satisfy (C.notInClass "\"\\")

pEscape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
  where decode c r = r <$ char c
-------------------------------------------------------------------------------

pKeyValuePair :: Parser (INIKey, INIVal)
pKeyValuePair = do
  key <- takeWhile (inClass "a-zA-Z0-9-")
  pSpaces
  eq <-  peekChar
  case eq of
    Just '=' -> do char '='
                   val <- pValue
                   return (key, val)
    _        -> return (key, IBool True)

pValue :: Parser INIVal
pValue = pSpaces *> val
  where val =  IBool   <$> pBool
           -- <|> IInt    <$> pInt
           -- <|> IString <$> pString
           -- <?> "value for the key"

pBool :: Parser Bool
pBool  =  False <$ pFalse
      <|> True  <$ pTrue

pTrue  =  try (p_strToByte "on")
      <|> try (p_strToByte "true")
      <|> try (p_strToByte "yes")

pFalse =  try (p_strToByte "off")
      <|> try (p_strToByte "false")
      <|> try (p_strToByte "no")

pInt :: Parser Integer
pInt = undefined

pString :: Parser B.ByteString
pString = undefined

-------------------------------------------------------------------------------
pSectEntry :: Parser (INISectName, INISection)
pSectEntry = (,) <$> pSectName <*> pSectMap
  where pSectMap = M.fromList <$> fmap groupTuple (many pKeyValuePair)

groupTuple :: Ord a => [(a, b)] -> [(a, [b])]
groupTuple xs = M.toList $ M.fromListWith (++) [(k, [v]) | (k, v) <- xs]

pINIFile :: Parser INIFile
pINIFile = M.fromList <$> (many pSectEntry <* takeByteString)

main_test =  parseOnly pINIFile
