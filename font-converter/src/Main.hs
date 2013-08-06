 
module Main where

import Data.Char
import Data.List.Split ( linesBy )

import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( withFile, IOMode(..) )

import Graphics.SVGFonts.ReadFont ( outlMap )

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fontFile:fontModule:fontBinding:_) -> 
      writeHaskellFont fontFile fontModule fontBinding
    (fontFile:fontModule:_) -> 
      writeHaskellFont fontFile fontModule defaultFontBinding
    _ -> displayHelp
  exitSuccess

-- | Haskell identifier to bind the translated font to. 
defaultFontBinding :: String
defaultFontBinding = "font"

writeHaskellFont :: FilePath -> String -> String -> IO ()
writeHaskellFont fontFile fontModule fontBinding = do
  check (isBindingName fontBinding) "The font binding name is invalid!"
  let modulePath = linesBy (== '.') fontModule
  check (isModulePath modulePath) "The font module is invalid!"
  let font@(fontData, outlines) = outlMap fontFile
  let outputFile = last modulePath ++ ".hs"
  withFile outputFile WriteMode $ \h -> do
    -- TODO
    return ()
  return ()

-- | Display the application help text.
displayHelp :: IO ()
displayHelp = do
  putStrLn "SVGFontConverter <input-svg-font> <font-module> [<font-binding-name>]"
  putStrLn "  input-svg-font    : The input SVG font file to be converted."
  putStrLn "  font-module       : The module name of the generated font."
  putStrLn "  font-binding-name : The identifier to bind the generated font to."

-- -----------------------------------------------------------------------
-- General utilities
-- -----------------------------------------------------------------------

-- | If the boolean condition does not hold exit the application with a failure
--   and display the given message. Otherwise proceed.
check :: Bool -> String -> IO ()
check True  _ = return ()
check False e = do
  putStrLn e
  exitFailure

-- | Replace all instances of @$n@, where @n@ is a positive integer,
--   with the n-th element in the given list. Example:
--   
-- > template "Hello $0, your last name is $1!" ["John", "Snow"] == "Hello John, your last name is Snow!"
--   
template :: String -> [String] -> Maybe String
template ('$':t) substs = 
  case reads t of
    ((i,tRest):_) | i >= 0 -> ((substs !! i) ++) `fmap` template tRest substs
    _ -> Nothing
template (c:t) substs = (c:) `fmap` template t substs
template [] _ = return []

-- -----------------------------------------------------------------------
-- String utilities for Haskell code
-- -----------------------------------------------------------------------

-- | Check if the list represents a valid module path, i.e. if @intercalate "." list@
--   is a valid module identifier.
isModulePath :: [String] -> Bool
isModulePath p = not (null p) && not (any null p) && all isModuleIdent p

-- | Check of the string is module identifier that may be used between the dots.
isModuleIdent :: String -> Bool
isModuleIdent s | not (null s) = (isUpper (head s) || isIdentSymbol (head s))
                              && all (\c -> isAlphaNum c || isIdentSymbol c) (tail s)
isModuleIdent _ = False

-- | Check if the character is a symbol that may appear in a haskell identifier.
isIdentSymbol :: Char -> Bool
isIdentSymbol '_'  = True
isIdentSymbol _ = False

-- | Check if the string is a valid haskell identifier for function bindings.
isBindingName :: String -> Bool
isBindingName s | not (null s) = (isLower (head s) || isIdentSymbol (head s))
                              && all (\c -> isAlphaNum c || isIdentSymbol c) (tail s)
isBindingName _ = False

