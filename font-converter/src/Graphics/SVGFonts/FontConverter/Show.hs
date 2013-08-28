 
module Graphics.SVGFonts.FontConverter.Show where 

import Data.List
import qualified Data.Map as M

import Diagrams.TwoD ( R2, unp2 )
import Diagrams.Path
import Diagrams.Trail
--import Diagrams.Segment
import Diagrams.Located

import Graphics.SVGFonts.ReadFont ( outlMap, FontData(..), Kern(..), OutlineMap )
import Graphics.SVGFonts.FontConverter.Types
import Graphics.SVGFonts.FontConverter.Utils

-- | Applies 'template' with the given substitutions on each line of the writer.
templateLW :: [String] -> LineWriter a -> LineWriter a
templateLW ts w = do
  let (x, ls) = runLW w
  mapM_ (line . template ts) ls
  return x

showRecord :: String -> [(String, LineWriter ())] -> LineWriter ()
showRecord name [] = line $ name ++ " {}"
showRecord name ((n,v):vs) = do
    line name
    indent $ do
      line $ "{ " ++ n ++ " = "
      indent $ indent $ v
      (flip mapM_) vs $ \(n',v') -> do
        line $ ", " ++ n' ++ " = "
        indent $ indent $ v'
      line $ "}"

showBinding :: (String, String) -> LineWriter () -> LineWriter ()
showBinding (n, t) v = do
  line $ n ++ " :: " ++ t
  line $ n ++ " = "
  indent $ v

showImports :: [String] -> LineWriter ()
showImports = mapM_ (line . ("import " ++))

showQualifiedImport :: (String, String) -> LineWriter ()
showQualifiedImport (m,n) = line $ "import qualified " ++ m ++ " as " ++ n

showTuple :: [LineWriter ()] -> LineWriter ()
showTuple [] = line $ "()"
showTuple (v:vs) = do
  line $ "("
  indent $ v
  (flip mapM_) vs $ \v' -> do
    line $ ","
    indent $ v'
  line $ ")"

showDiagramsImports :: LineWriter ()
showDiagramsImports = showImports 
  [ "Diagrams.TwoD"
  , "Diagrams.Coordinates"
  , "Diagrams.Segment"
  , "Diagrams.Trail"
  , "Diagrams.Path"
  , "Diagrams.Located" ]

showMapImport :: LineWriter ()
showMapImport = showQualifiedImport ("Data.Map", "M")

outlineModuleHead :: String -> [String] -> LineWriter ()
outlineModuleHead m exports = do
  line $ "module " ++ m ++ " ( " ++ intercalate ", " exports ++ " ) where"
  showMapImport
  line $ "import qualified Graphics.SVGFonts.ReadFont as F"
  --line $ "import Data.FingerTree ( fromList )"
  showDiagramsImports

-- | @moduleHead m es ms@ shows the module head (with exports) where
--   @m@ is the module name and @es@ is the list of exported bindings.
--   @ms@ gives the list of modules paths that also shall be imported.
moduleHead :: String -> [String] -> [[String]] -> LineWriter ()
moduleHead m exports ms = do
  line $ "module " ++ m ++ " ( " ++ intercalate ", " exports ++ " ) where"
  line $ "import Data.Vector"
  showMapImport
  line $ "import qualified Graphics.SVGFonts.ReadFont as F"
  showImports $ fmap (intercalate ".") $ ms

showConversionFunction :: LineWriter ()
showConversionFunction = do
  line $ "glyphsToOutlines :: F.SvgGlyphs -> F.OutlineMap"
  line $ "glyphsToOutlines glyphs = (flip M.mapWithKey) glyphs $ "
  line $ "  \\ch _ -> mconcat $ F.commandsToTrails (F.commands ch glyphs) [] zeroV zeroV zeroV"

-- | @showFontModule mp e ms fd om@ shows the central font module. @mp@ is the
--   module path and @e@ gives the binding for the font. @fd@ shows the font data
--   and @om@ shows the outline map. @ms@ gives a list of modules paths that also have to 
--   be imported.
showFontModule :: [String] -> String -> [[String]] -> LineWriter () -> LineWriter () -> LineWriter ()
showFontModule modulePath export ms fontData outlines = do
  moduleHead (intercalate "." modulePath) [export] ms
  showImports ["Data.Monoid", "Data.AdditiveGroup"]
  showConversionFunction
  showBinding (export, "(F.FontData, F.OutlineMap)") $ do
    showLet [ ("fontData", fontData), ("outlines", outlines)] $ do
      line $ "(fontData, outlines)"

-- | @showOutlineMapModule mp e om@ - @mp@ module path, @e@ binding export, @om@ outline map;
showOutlineMapModule :: [String] -> String -> LineWriter () -> LineWriter ()
showOutlineMapModule modulePath export outlines = do
  outlineModuleHead (intercalate "." modulePath) [export]
  showBinding (export, "F.OutlineMap") outlines

-- | @showGlyphMapModule mp e om@ - @mp@ module path, @e@ binding export, @om@ outline map;
showGlyphMapModule :: [String] -> String -> LineWriter () -> LineWriter ()
showGlyphMapModule modulePath export glyphs = do
  outlineModuleHead (intercalate "." modulePath) [export]
  showBinding (export, "M.Map String (String, Double, String)") glyphs

-- | @showKerningModule mp e k@ - @mp@ module path, @e@ binding export, @om@ kernings;
showKerningModule :: [String] -> String -> Kern -> LineWriter ()
showKerningModule modulePath export k = do
  moduleHead (intercalate "." modulePath) [export] []
  showBinding (export, "F.Kern") $ showKerning k

showKerning :: Kern -> LineWriter ()
showKerning k = showRecord "F.Kern" $ 
  [ ("F.kernU1S", showMap $ kernU1S $ k)
  , ("F.kernU2S", showMap $ kernU2S $ k)
  , ("F.kernG1S", showMap $ kernG1S $ k)
  , ("F.kernG2S", showMap $ kernG2S $ k)
  , ("F.kernK", showVal $ kernK $ k)
  ]

showBindModule :: [String] -> (String, String) -> LineWriter () -> LineWriter ()
showBindModule modulePath export@(n,_) w = do
  moduleHead (intercalate "." modulePath) [n] []
  showBinding export w

makeMapLists :: (Show k) => (v -> LineWriter ()) -> M.Map k v -> [LineWriter ()]
makeMapLists f m = do
  l <- divideList dataPerFile (M.toList m)
  return $ do
    line "M.fromList $"
    indent $ showList'' (showEntry f) l

divideList :: Int -> [a] -> [[a]]
divideList _ [] = []
divideList n xs = 
  let (front, back) = splitAt n xs
  in front : divideList n back

showLet :: [(String, LineWriter ())] -> LineWriter () -> LineWriter ()
showLet [] v = v
showLet ((n,b):bs) v = do
    line $ "let " ++ n ++ " ="
    indent $ indent $ do
      indent $ b
      (flip mapM_) bs $ \(n',b') -> do
        line $ n' ++ " ="
        indent $ b'
    line $ "in"
    indent $ v

showList' :: (a -> String) -> [a] -> LineWriter ()
showList' _ [] = line "[]"
showList' f (v:vs) = do
    line $ "[ " ++ f v
    showListHelper f vs
  where
    showListHelper :: (a -> String) -> [a] -> LineWriter ()
    showListHelper _ [] = line $ "]"
    showListHelper f' (v':vs') = do
      line $ ", " ++ f' v'
      showListHelper f' vs'

showList'' :: (a -> LineWriter ()) -> [a] -> LineWriter ()
showList'' _ [] = line "[]"
showList'' f (v:vs) = do
  line $ "["
  f v
  mapM_ (\x -> line "," >> indent (f x)) vs
  line $ "]"

showMap :: (Show k, Show v) => M.Map k v -> LineWriter ()
showMap m = indent $ do
  line "M.fromList $"
  indent $ showList' show $ M.toList m

showMap' :: (Show k) => (v -> LineWriter ()) -> M.Map k v -> LineWriter ()
showMap' f m = indent $ do
    line "M.fromList $"
    showList'' (showEntry f) (M.toList m)

showMapUnion :: [LineWriter ()] -> LineWriter ()
showMapUnion (m:ms) = do
  line $ "( "
  indent $ m
  mapM_ (line "`M.union`" >>) ms
  line $ ")"

showEntry :: (Show k) => (v -> LineWriter ()) -> (k,v) -> LineWriter ()
showEntry f (k,v) = showTuple [line $ show k, f v]

showVal :: (Show v) => v -> LineWriter ()
showVal = line . show

showPath :: Path R2 -> LineWriter ()
showPath p = showRecord "Path" [("pathTrails", showList'' showLoc $ pathTrails p)]
  where
    showLoc :: Located (Trail R2) -> LineWriter ()
    showLoc l = do
      let (pos, t) = viewLoc l
      line $ "at ("
      indent $ showTrail t
      line $ ") (p2 " ++ show (unp2 pos) ++ ")"
    showTrail :: Trail R2 -> LineWriter ()
    showTrail = showVal

-- | @showFontData fd gs ks@ shows the given font data @fd@ using the
--   @gs@ as the glyphs and @ks@ as the kernings.
showFontData :: FontData -> LineWriter () -> LineWriter () -> LineWriter ()
showFontData fontData glyphs kernings = showRecord "F.FontData" $
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




