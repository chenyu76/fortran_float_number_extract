{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Bits ((.&.))
import qualified Data.ByteString.Char8 as B
import Data.Generics.Uniplate.Operations (universeBi)
import Data.List (isSuffixOf)
import Language.Fortran.AST
import Language.Fortran.AST.Literal.Real (Exponent (..), ExponentLetter (..), RealLit (..))
import Language.Fortran.Parser (byVerFromFilename, f2003, f77e, f77l, f90)
import Language.Fortran.Util.Position
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath (takeExtension, (</>))
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [dir] -> scanDir dir
    _ -> putStrLn "Usage: fortran_num_extract <directory>"

fortranExtensions :: [String]
fortranExtensions =
  [ ".f",
    ".F",
    ".for",
    ".FOR",
    ".f90",
    ".F90",
    ".f95",
    ".F95",
    ".f03",
    ".F03",
    ".f08",
    ".F08",
    ".f18",
    ".F18",
    ".ftn",
    ".FTN"
  ]

scanDir :: FilePath -> IO ()
scanDir dir = do
  absDir <- canonicalizePath dir
  isDir <- doesDirectoryExist absDir
  if not isDir
    then putStrLn $ "Error: " ++ absDir ++ " is not a directory"
    else do
      files <- findFortranFiles absDir
      results <- concat <$> mapM processFile files
      mapM_ putStrLn results

findFortranFiles :: FilePath -> IO [FilePath]
findFortranFiles dir = do
  entries <- listDirectory dir
  let paths = map (dir </>) entries
  concat <$> mapM classifyPath paths

classifyPath :: FilePath -> IO [FilePath]
classifyPath p = do
  isFile <- doesFileExist p
  isDir <- doesDirectoryExist p
  if isFile && isFortranFile p
    then return [p]
    else
      if isDir
        then findFortranFiles p
        else return []

isFortranFile :: FilePath -> Bool
isFortranFile file = any (`isSuffixOf` takeExtension file) fortranExtensions

processFile :: FilePath -> IO [String]
processFile file = do
  content <- B.readFile file
  return $ formatReport file content

formatReport :: FilePath -> B.ByteString -> [String]
formatReport file content = case tryParse file content of
  Just pf -> formatLiterals file content pf
  Nothing -> []

tryParse :: FilePath -> B.ByteString -> Maybe (ProgramFile A0)
tryParse file content =
  firstSuccess
    [ byVerFromFilename file content,
      f2003 file content,
      f90 file content,
      f77e file content,
      f77l file content
    ]

firstSuccess :: [Either e a] -> Maybe a
firstSuccess [] = Nothing
firstSuccess (Right x : _) = Just x
firstSuccess (Left _ : xs) = firstSuccess xs

formatLiterals :: FilePath -> B.ByteString -> ProgramFile A0 -> [String]
formatLiterals file content pf =
  [ file ++ ":" ++ show line ++ ":" ++ show col ++ ": " ++ valText
    | (span_, valText) <- findLiterals content pf,
      let pos = ssFrom span_
          line = posLine pos
          col = posColumn pos - 1
  ]

findLiterals :: B.ByteString -> ProgramFile A0 -> [(SrcSpan, String)]
findLiterals content pf =
  [ (span_, text)
    | ExpValue _ span_ val <- universeBi pf,
      isWithoutPrecision val,
      let text = extractText content span_,
      not (isBinaryRepresentable text)
  ]

extractText :: B.ByteString -> SrcSpan -> String
extractText bs (SrcSpan from to) =
  let start = posAbsoluteOffset from
      end = posAbsoluteOffset to
   in B.unpack $ B.take (end - start + 1) $ B.drop start bs

isWithoutPrecision :: Value A0 -> Bool
isWithoutPrecision = \case
  ValInteger _ Nothing -> True
  ValReal (RealLit _ (Exponent ExpLetterE _)) Nothing -> True
  _ -> False

isBinaryRepresentable :: String -> Bool
isBinaryRepresentable s = case parseFortranReal s of
  Just (num, den) ->
    let d = den `div` gcd num den
     in isPowerOfTwo d
  Nothing -> True

isPowerOfTwo :: Integer -> Bool
isPowerOfTwo n = n > 0 && (n .&. (n - 1)) == 0

parseFortranReal :: String -> Maybe (Integer, Integer)
parseFortranReal s = do
  let (sigStr, expStr) = breakExponent s
  (sigNum, sigDen) <- parseDecimal sigStr
  expVal <- if null expStr then Just (0 :: Integer) else readMaybe expStr
  let (num, den) =
        if expVal >= 0
          then (sigNum * 10 ^ expVal, sigDen)
          else (sigNum, sigDen * 10 ^ negate expVal)
  return (num, den)

breakExponent :: String -> (String, String)
breakExponent s = case break (\c -> c `elem` ("eEdD" :: String)) s of
  (sig, []) -> (sig, "")
  (sig, _ : rest) -> (sig, rest)

parseDecimal :: String -> Maybe (Integer, Integer)
parseDecimal s = case break (== '.') s of
  (intStr, "") -> do
    intVal <- readMaybe intStr
    return (intVal, 1)
  (intStr, _ : fracStr) -> do
    let intStr' = if null intStr then "0" else intStr
    intVal <- readMaybe intStr'
    let fracDigits = length fracStr
        fracVal = if null fracStr then 0 else read fracStr
        num = intVal * 10 ^ fracDigits + fracVal
        den = 10 ^ fracDigits
    return (num, den)
