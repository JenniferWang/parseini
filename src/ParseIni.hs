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
    ) where

import Prelude hiding (takeWhile, take, concat)
import Data.ByteString.Char8 (pack, concat)
import Data.Attoparsec.ByteString.Char8(char, peekChar, anyChar)
import qualified Data.Attoparsec.ByteString.Char8 as C
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import Data.Attoparsec.ByteString
import Control.Applicative
import Data.Char (toLower, toUpper)
import Data.Bits (shift)
import GHC.Word (Word8)

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
parseIniFile = parseOnly pINIFile

-------------------------------------------------------------------------------
-- Helper Functions

pStrToByte :: String -> Parser B.ByteString
pStrToByte = string . pack

pEmpty :: Parser B.ByteString
pEmpty = pStrToByte ""

isEOL :: Word8 -> Bool
isEOL = inClass "\n"

pEOL :: Parser ()
pEOL = skipMany (pStrToByte "\n")

pEOL1 :: Parser ()
pEOL1 = skipMany1 (pStrToByte "\n")

between :: Parser open -> Parser close -> Parser a -> Parser a
between open close a = open *> a <* close

pSkipRestOfLine :: Parser ()
pSkipRestOfLine = skipWhile (\x -> not (isEOL x)) *> pEOL

pSpaces :: Parser B.ByteString
pSpaces = takeWhile (inClass " ")

pEscape :: Parser Char
pEscape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
  where decode c r = r <$ char c

-- TODO: this is a very ugly implementation
pEscapeByte :: Parser B.ByteString
pEscapeByte = pack <$> do c <- pEscape
                          return [c]

pComment :: Parser ()
pComment =  char '#' *> pSkipRestOfLine
        <|> char ';' *> pSkipRestOfLine

-------------------------------------------------------------------------------
-- Section Header

pSectName :: Parser INISectName
pSectName = do
  names <- (between (char '[' <* pSpaces) (pSpaces *> char ']') pNames)
  return $ toSectName (fst names) (snd names)

pNames :: Parser (String, Maybe String)
pNames = do
  name1 <- (pName <* pSpaces)
  c     <- peekChar
  if (c == Just '\"')
     then do name2 <- between (char '\"')
                      (char '\"' <* pSpaces)
                      pSubName
             return (name1, Just name2)
     else return (name1, Nothing)

pName :: Parser String
pName = many (C.satisfy (C.inClass "a-zA-Z0-9-."))

pSubName :: Parser String
pSubName = many namechar
  where namechar = (char '\\' *> pEscape)
                  <|> C.satisfy (C.notInClass "\"\\")
-------------------------------------------------------------------------------
-- Variables and Values

pKeyValuePair :: Parser (INIKey, INIVal)
pKeyValuePair = do
  key <- takeWhile1 (inClass "a-zA-Z0-9-")
  eq  <- (pSpaces *> peekChar)
  case eq of
    Just '=' -> do anyChar
                   val <- pValue
                   return (key, val)
    _        -> return (key, IBool True)

pSpaceAndBackSlash :: Parser ()
pSpaceAndBackSlash = skipMany (try (pStrToByte " ")
                          <|> try (pStrToByte "\\\n"))

pValue :: Parser INIVal
pValue = pSpaceAndBackSlash *> val
  where val =  IBool   <$> (pBool <* go)
           <|> IInt    <$> (pInt <* go)
           <|> IString <$> pString
           <?> "value for the key"
        go = pSpaceAndBackSlash *> choice [pComment, pEOL1]

pBool :: Parser Bool
pBool  =  False <$ pFalse
      <|> True  <$ pTrue

pCaseInsensitiveChar :: Char -> Parser Char
pCaseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

pCaseInsensitiveString :: String -> Parser String
pCaseInsensitiveString str = mapM pCaseInsensitiveChar str

pTrue :: Parser String
pTrue  =  try (pCaseInsensitiveString "on")
      <|> try (pCaseInsensitiveString "true")
      <|> try (pCaseInsensitiveString "yes")

pFalse :: Parser String
pFalse =  try (pCaseInsensitiveString "off")
      <|> try (pCaseInsensitiveString "false")
      <|> try (pCaseInsensitiveString "no")

pInt :: Parser Integer
pInt = do
  sign <- peekChar
  case sign
    of Just '-' -> do anyChar
                      number <- pSuffixNumber
                      return (-number)
       Just '+' -> anyChar >> pSuffixNumber
       _        -> pSuffixNumber

pSuffixNumber :: Parser Integer
pSuffixNumber =  try pSuffixK
             <|> try pSuffixM
             <|> try pSuffixG
             <|> try pSuffixT
             <|> try pSuffixP
             <|> try pSuffixE
             <|> try C.decimal
  where
    pSuffixK = do number <- C.decimal; char 'k'; return (number * shift 1 10)
    pSuffixM = do number <- C.decimal; char 'M'; return (number * shift 1 20)
    pSuffixG = do number <- C.decimal; char 'G'; return (number * shift 1 30)
    pSuffixT = do number <- C.decimal; char 'T'; return (number * shift 1 40)
    pSuffixP = do number <- C.decimal; char 'P'; return (number * shift 1 50)
    pSuffixE = do number <- C.decimal; char 'E'; return (number * shift 1 60)

pString :: Parser B.ByteString
pString = do
  str <- takeWhile (notInClass "\"\\\n#; ")
  sym <- peekChar
  case sym of Nothing   -> return str
              Just '\n' -> return str
              Just s    -> do rest <- (pSpecialCharInString s)
                              return (B.append str rest)

pSpecialCharInString :: Char -> Parser B.ByteString
pSpecialCharInString c
  | c == '\\' = do escape <- pBackSlash
                   rest <- pString
                   return (B.append escape rest)
  | c == '\"' = do quoted <- pQuoted
                   rest <- pString
                   return (B.append quoted rest)
  | c == ' '  = do sp <- pInterSpaces
                   rest <- pString
                   if (rest == pack "")
                      then return rest
                      else return (B.append sp rest)
  | c `elem` ['#', ';'] = pSkipRestOfLine *> pEmpty
  | otherwise = return (pack "You hit the wrong place")

pBackSlash :: Parser B.ByteString
pBackSlash =  try (pStrToByte "\\\n" *> pEmpty)
          <|> try (pStrToByte "\\" *> pEscapeByte)

pInterSpaces :: Parser B.ByteString
pInterSpaces = do
  sp <- pSpaces
  next <- peekChar
  if (next `elem` [Just '\n', Just ';', Just '#'])
     then pSkipRestOfLine >> return (pack "")
     else return sp

pQuoted :: Parser B.ByteString
pQuoted = between (char '\"') (char '\"') pContent
  where pContent = concat <$> many'
          (  pStrToByte "\\\n" *> pEmpty
         <|> pStrToByte "\\" *> pEscapeByte
         <|> (B.singleton <$> satisfy (notInClass "\"\\"))
          )

-------------------------------------------------------------------------------
-- Section Entry

pSectEntry :: Parser (INISectName, INISection)
pSectEntry = do
  name <- (skips *> pSectName <* skips)
  kvs  <- many $ (pKeyValuePair <* skips)
  let sect_map = M.fromList (groupTuple kvs)
  return (name, sect_map)
    where skips = do
            x <- peekChar
            case x of Just '\n'-> anyChar >> skips
                      Just ' ' -> anyChar >> skips
                      Just ';' -> pComment >> skips
                      Just '#' -> pComment >> skips
                      _        -> return ()

groupTuple :: Ord a => [(a, b)] -> [(a, [b])]
groupTuple xs = M.toList $ M.fromListWith (++) [(k, [v]) | (k, v) <- xs]

pINIFile :: Parser INIFile
pINIFile = M.fromList <$> (many pSectEntry <* pEOL)

