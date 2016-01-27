module Main
    ( main
    ) where

import ParseIni
import PrettyPrintIni

import Control.Monad (liftM)
import Data.ByteString.Char8 as B
import Data.Map.Strict as M
import Test.Hspec
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Property (failed, liftBool, Result)

-- |Run all tests.
main :: IO ()
main = hspec $ describe "Testing parser" $ do
    -- Check that an empty file returns an empty map.
    it "Empty file" $ parseIniFile B.empty `shouldBe` Right M.empty

    -- Check that a suffixed number is correctly interpreted.
    it "Interpret suffixed number" $
      testSingleCase "[sec]\nkey=-1M\n" "[sec]\n    key = -1048576\n"

    -- Check that space is handled correctly
    it "Section name and key-value appear in one line" $
      testSingleCase "[sec] key = val\n" "[sec]\n    key = \"val\"\n"

    -- Check that value with escapes are handled correctly
    it "Check escapes" $
      testSingleCase "[sec]\nkey = value with escapes\\\\ \\\" \\n\\t \\\"\n"
                     "[sec]\n    key = \"value with escapes\\\\ \\\" \\n\\t \\\"\"\n"

    it "Check escapes in subsection name" $
      testSingleCase "[include \"Sub section \\\" \"]\n  var = true\n"
                     "[include \"Sub section \\\" \"]\n    var = True\n"

    it "Check keep internal spaces" $
      testSingleCase "[sec]\nkey =   keep  internal    spaces\n"
                     "[sec]\n    key = \"keep  internal    spaces\"\n"

    it "Check escape comments" $
      testSingleCase "#ignore ignore\n[sec] ;ignore\n \nkey=value #ignore\n"
                     "[sec]\n    key = \"value\"\n"

    it "Check partially quoted string" $
      testSingleCase "[sec]\nkey = a partially  \"  quoted\n    \" string"                                      "[sec]\n    key = \"a partially    quoted\\n     string\"\n"

    it "Check escape newline and trailing whitespaces" $
      testSingleCaseForMultipleEntries
        "[section]\nkey = value with \\\n      escaped newline\n\n    key2=\"also has an \\\nescaped newline\"     \\\n    ; escaped newlines like the above are removed during parsing\n    ; and remember to strip trailing whitespace\n    ; and comments, including trailing whitespace\n    ; in continued lines!\n"
        [ "[section]\n    key = \"value with       escaped newline\"\n    key2 = \"also has an escaped newline\"\n"
        , "[section]\n    key2 = \"also has an escaped newline\"\n    key = \"value with       escaped newline\"\n"
        ]

    it "Check case sensitivity and line continuation" $
      testSingleCase
        "[SectionIsCaseInsensitive     \"SUBSectionIsCaseSensitive\"]\nkey =   \\\n    FAlse \\\n    # line continuations are allowed in any value;\n    # remember to strip trailing whitespace and\n    # comments before parsing!"
        "[sectioniscaseinsensitive \"SUBSectionIsCaseSensitive\"]\n    key = False\n"

    it "Check no line continuation in bool or number 1" $
      testSingleCase "[sec]\nkey =   \\\n    FA\\\n    lse \\\n"
                     "[sec]\n    key = \"FA    lse\"\n"

    it "Check no line continuation in bool or number 2" $
      testSingleCase "[sec]\nkey =  1000\\\nM\n"
                     "[sec]\n    key = \"1000M\"\n"

    -- Check that the pretty printer is idempotent through parsing,
    -- and that the parse returns the correct result.
    it "Pretty-printer idempotence" $ testIdemp B.empty `shouldBe` Right M.empty
    -- *NOTE* that the above test only checks for the empty case.
    -- Once you have implemented your parser and pretty-printer,
    -- you should check for idempotence on more interesting cases.

    -- Check that the parser correctly saves every value for a multivalued variable.
    it "Correct count for multivalued variables" $ property pMultivalCount
    -- The above test will fail until you've implemented your parser.

    -- You will need more tests, both hspec and QuickCheck based.
    -- Hint: think about how you can generate random INIFiles using your
    -- pretty printer and QuickCheck, and/or by writing a grammar for
    -- generating INI files directly.

-- |Test the pretty-printer by checking for idempotence, then returning parsed result.
testIdemp :: B.ByteString -> Either String INIFile
testIdemp s | run1 == run2 = parseRes
            | otherwise = errorRes
  where run1 = liftM prettyPrint $ parseIniFile s
        run2 = liftM prettyPrint $ parseIniFile =<< run1
        parseRes = parseIniFile =<< run2
        errorRes = Left "Pretty printer failed idempotence check."

-- |Look up a value in an INIFile.
lookupVal :: INISectName -> INIKey -> INIFile -> [INIVal]
lookupVal sect key file = M.findWithDefault [] key $ M.findWithDefault M.empty sect file

testSingleCase :: String -> String -> Bool
testSingleCase str_in str_out =
  case run of Left _    -> False
              Right ini -> (prettyPrint ini) == (pack str_out)
    where run = parseIniFile (pack str_in)

testSingleCaseForMultipleEntries :: String -> [String] -> Bool
testSingleCaseForMultipleEntries str_in strs_out =
  or $ Prelude.map (testSingleCase str_in) strs_out

-- **** QuickCheck tests ****
type MultivalCount = NonNegative Int

-- |Test that, given an INI file with some number of repeated @key=val@ declarations,
-- the resulting parse has that number of values in the @INIFile@.
pMultivalCount :: MultivalCount -> Result
pMultivalCount c | (Left _) <- eParsedOut = failed
                 | otherwise = lengthMatches
  where -- construct input stream that declares a multivalued variable
        (NonNegative cInt) = c
        keyDecls = Prelude.take cInt $ repeat "key=val\n"
        inFile = B.pack $ "[section]\n" ++ Prelude.concat keyDecls

        -- parse the generated input stream
        eParsedOut = parseIniFile inFile
        (Right parsedOut) = eParsedOut

        -- extract the key from the parsed output
        sect = toSectName "section" Nothing
        key = toKey "key"
        values = lookupVal sect key parsedOut

        -- length of values should match value of c
        lengthMatches = liftBool $ cInt == Prelude.length values
