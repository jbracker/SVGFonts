 
module Main where

import Data.Char
import Data.List
import Data.List.Split ( linesBy )
import qualified Data.Map as M

import qualified Control.Monad.Writer as W

import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( withFile, hPutStrLn, hFlush, IOMode(..), Handle )

import Graphics.SVGFonts.ReadFont ( outlMap, FontData(..), Kern(..), OutlineMap )

import Diagrams.TwoD ( R2, unp2 )
import Diagrams.Path
import Diagrams.Trail
import Diagrams.Segment
import Diagrams.Located

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
  let (fontData, outlines) = outlMap fontFile
  let fontModuleFile = intercalate "/" fontModulePath ++ ".hs"
  withFile fontModuleFile WriteMode $ \h -> write h $ do
    showFontModule fontModulePath fontBinding fontData outlines
  return ()

-- | Display the application help text.
displayHelp :: IO ()
displayHelp = do
  putStrLn "SVGFontConverter <input-svg-font> <font-module> [<font-binding-name>]"
  putStrLn "  input-svg-font    : The input SVG font file to be converted."
  putStrLn "  font-module       : The module name of the generated font."
  putStrLn "  font-binding-name : The identifier to bind the generated font to."

-- | @moduleHead m es@ shows the module head (with exports) where
--   @m@ is the module name and @es@ is the list of exported bindings. 
moduleHead :: String -> [String] -> LineWriter ()
moduleHead m exports = do
  line $ "module " ++ m ++ " ( " ++ intercalate ", " exports ++ " ) where"
  line $ "import Data.Map ( fromList )"
  line $ "import qualified Data.Map as M"
  line $ "import qualified Graphics.SVGFonts.ReadFont as F"
  line $ "import Diagrams.TwoD"
  line $ "import Diagrams.Coordinates"
  line $ "import Diagrams.Segment"
  line $ "import Diagrams.Trail"
  line $ "import Diagrams.Path"
  line $ "import Diagrams.Located"

showFontModule :: [String] -> String -> FontData -> OutlineMap -> LineWriter ()
showFontModule fontModule export fontData outlines = do
  let fontDataOut = showFontData fontData (mapVals $ fontDataGlyphs fontData) $ do
        line $ "F.Kern"
        indent $ recordVals $ 
          [ ("F.kernU1S", mapVals $ kernU1S $ fontDataKerning fontData)
          , ("F.kernU2S", mapVals $ kernU2S $ fontDataKerning fontData)
          , ("F.kernG1S", mapVals $ kernG1S $ fontDataKerning fontData)
          , ("F.kernG2S", mapVals $ kernG2S $ fontDataKerning fontData)
          , ("F.kernK", showVal $ kernK $ fontDataKerning fontData)
          ]
  let outlinesOut = mapVals' showPath outlines
  moduleHead (intercalate "." fontModule) [export]
  line $ export ++ " :: (F.FontData, F.OutlineMap)"
  line $ export ++ " = "
  indent $ letBinds [ ("fontData", fontDataOut), ("outlines", outlinesOut)] $ do
    line $ "(fontData, outlines)"

-- -----------------------------------------------------------------------
-- Writer Monad
-- -----------------------------------------------------------------------

newtype LineWriter a = LW { unLW :: W.Writer [String] a }

instance Functor LineWriter where
  fmap f = LW . fmap f . unLW

instance Monad LineWriter where
  return = LW . return
  (>>=) m f = LW $ unLW m >>= unLW . f

instance Show (LineWriter a) where
  show w = unlines $ W.execWriter $ unLW w

line :: String -> LineWriter ()
line s = LW $ W.tell [s]

indent :: LineWriter a -> LineWriter a
indent w = LW $ W.censor (fmap ("  "++)) $ unLW w

write :: Handle -> LineWriter a -> IO ()
write h w = do
  mapM_ (hPutStrLn h) (W.execWriter $ unLW w)
  hFlush h


recordVals :: [(String, LineWriter ())] -> LineWriter ()
recordVals [] = line "{}"
recordVals ((n,v):vs) = do
    line $ "{ " ++ n ++ " = "
    indent $ v 
    recordVals' vs
  where
    recordVals' :: [(String, LineWriter ())] -> LineWriter ()
    recordVals' [] = line $ "}"
    recordVals' ((n',v'):vs') = do
      line $ ", " ++ n' ++ " = "
      indent $ v'
      recordVals' vs'

letBinds :: [(String, LineWriter ())] -> LineWriter () -> LineWriter ()
letBinds [] v = v
letBinds ((n,b):bs) v = do
    line $ "let " ++ n ++ " ="
    indent $ indent $ indent $ b
    sequence_ $ fmap binding bs
    line $ "in"
    indent v
  where
    binding :: (String, LineWriter ()) -> LineWriter ()
    binding (n', b') = do
      indent $ indent $ line $ n' ++ " ="
      indent $ indent $ indent $ b'

listVals :: (a -> String) -> [a] -> LineWriter ()
listVals _ [] = line "[]"
listVals f (v:vs) = do
    line $ "[ " ++ f v
    showListHelper f vs
  where
    showListHelper :: (a -> String) -> [a] -> LineWriter ()
    showListHelper _ [] = line $ "]"
    showListHelper f' (v':vs') = do
      line $ ", " ++ f' v'
      showListHelper f' vs'

listVals' :: (a -> LineWriter ()) -> [a] -> LineWriter ()
listVals' _ [] = line "[]"
listVals' f (v:vs) = do
  line $ "["
  f v
  mapM_ (\x -> line "," >> indent (f x)) vs
  line $ "]"

mapVals :: (Show k, Show v) => M.Map k v -> LineWriter ()
mapVals m = indent $ do
  line "M.fromList $"
  indent $ listVals show $ M.toList m

mapVals' :: (Show k) => (v -> LineWriter ()) -> M.Map k v -> LineWriter ()
mapVals' f m = indent $ do
    line "M.fromList $"
    listVals' (showEntry f) (M.toList m)
  where
    showEntry :: (Show k) => (v -> LineWriter ()) -> (k,v) -> LineWriter ()
    showEntry f' (k,v) = do
      line $ "( " ++ show k ++ ", "
      indent $ f' v
      line $ ")"

showVal :: (Show v) => v -> LineWriter ()
showVal = indent . line . show

showPath :: Path R2 -> LineWriter ()
showPath p = do
    line "Path { pathTrails = "
    indent $ listVals' showLoc $ pathTrails p
    line "}"
  where
    showLoc :: Located (Trail R2) -> LineWriter ()
    showLoc l = do
      let (pos, t) = viewLoc l
      line $ "at ("
      indent $ showTrail t
      line $ ") (p2 " ++ show (unp2 pos) ++ ")"
    showTrail :: Trail R2 -> LineWriter ()
    showTrail = line . show

-- | @showFontData fd gs ks@ shows the given font data @fd@ using the
--   @gs@ as the glyphs and @ks@ as the kernings.
showFontData :: FontData -> LineWriter () -> LineWriter () -> LineWriter ()
showFontData fontData glyphs kernings = do
  line $ "F.FontData"
  indent $ recordVals $
    [ ("F.fontDataGlyphs", glyphs)
    , ("F.fontDataKerning", kernings)
    , ("F.fontDataBoundingBox", showVal (fontDataBoundingBox fontData))
    , ("F.fontDataFileName", showVal (fontDataFileName fontData))
    , ("F.fontDataUnderlinePos", showVal (fontDataUnderlinePos fontData))
    , ("F.fontDataUnderlineThickness", showVal (fontDataUnderlineThickness fontData))
    , ("F.fontDataHorizontalAdvance", showVal (fontDataHorizontalAdvance fontData))
    , ("F.fontDataFamily", showVal (fontDataFamily fontData))
    , ("F.fontDataWeight", showVal (fontDataWeight fontData))
    , ("F.fontDataStretch", showVal (fontDataStretch fontData))
    , ("F.fontDataUnitsPerEm", showVal (fontDataUnitsPerEm fontData))
    , ("F.fontDataPanose", showVal (fontDataPanose fontData))
    , ("F.fontDataAscent", showVal (fontDataAscent fontData))
    , ("F.fontDataDescent", showVal (fontDataDescent fontData))
    , ("F.fontDataXHeight", showVal (fontDataXHeight fontData))
    , ("F.fontDataCapHeight", showVal (fontDataCapHeight fontData))
    , ("F.fontDataHorizontalStem", showVal (fontDataHorizontalStem fontData))
    , ("F.fontDataVerticalStem", showVal (fontDataVerticalStem fontData))
    , ("F.fontDataUnicodeRange", showVal (fontDataUnicodeRange fontData))
    ]


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

