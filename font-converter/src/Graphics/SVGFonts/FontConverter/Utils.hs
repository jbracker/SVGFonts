
module Graphics.SVGFonts.FontConverter.Utils where  

import System.Exit ( exitFailure, exitSuccess )

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