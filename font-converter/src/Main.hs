module Main where

import Data.Char
import Data.List
import Data.List.Split ( linesBy )
import Data.String


import System.Environment ( getArgs )
import System.Exit ( exitSuccess )

import qualified Shelly as S

import Graphics.SVGFonts.ReadFont ( outlMap, FontData(..), Kern(..), OutlineMap )
import Graphics.SVGFonts.FontConverter.Types
import Graphics.SVGFonts.FontConverter.Show
import Graphics.SVGFonts.FontConverter.Utils

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
writeHaskellFont fontFile fontModuleName fontBinding = do
  check (isBindingName fontBinding) "The font binding name is invalid!"
  let fontModulePath = linesBy (== '.') fontModuleName
  check (isModulePath fontModulePath) "The font module is invalid!"
  let outlineModulePath = fontModulePath ++ ["Outlines"]
  let glyphsModulePath = fontModulePath ++ ["Glyphs"]
  let kerningModulePath = fontModulePath ++ ["Kernings"]
  let (fontData, outlines) = outlMap fontFile
  let outlineParts = 
        (fmap ((fontModulePath++).return.("Outlines"++).show) [1::Int ..]) `zip` 
        makeMapLists showPath outlines
  let glyphParts = 
        (fmap ((fontModulePath++).return.("Glyphs"++).show) [1::Int ..]) `zip` 
        makeMapLists showVal (fontDataGlyphs fontData)
  let fontDataOut = showFontData fontData 
        (showMapUnion $ fmap (line.(++".glyphs").(intercalate ".").fst) glyphParts) 
        (line "kernings")
  S.shelly $ do
    S.mkdir_p $ concatPath fontModulePath
  (flip mapM_) outlineParts $ \(modulePath, w) -> do
    writeModule modulePath $ do
      showOutlineMapModule modulePath "outlines" w
  (flip mapM_) glyphParts $ \(modulePath, w) -> do
    writeModule modulePath $ do
      showGlyphMapModule modulePath "glyphs" w
  writeModule kerningModulePath $ do
    showKerningModule kerningModulePath "kernings" (fontDataKerning fontData)
  writeModule fontModulePath $ do
    showFontModule fontModulePath fontBinding 
      ([kerningModulePath] ++ fmap fst outlineParts ++ fmap fst glyphParts)
      fontDataOut
      (showMapUnion $ fmap (line.(++".outlines").(intercalate ".").fst) outlineParts)
  return ()

concatPath :: [String] -> S.FilePath
concatPath [] = fromString "."
concatPath (p:[]) = fromString p
concatPath (p:ps) = p S.</> concatPath ps

writeModule :: [String] -> LineWriter a -> IO a
writeModule modulePath w = do
  writeToFile (intercalate "/" modulePath ++ ".hs") w

-- | Display the application help text.
displayHelp :: IO ()
displayHelp = do
  putStrLn "SVGFontConverter <input-svg-font> <font-module> [<font-binding-name>]"
  putStrLn "  input-svg-font    : The input SVG font file to be converted."
  putStrLn "  font-module       : The module name of the generated font."
  putStrLn "  font-binding-name : The identifier to bind the generated font to."






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

