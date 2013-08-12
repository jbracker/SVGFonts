
module Graphics.SVGFonts.FontConverter.Utils where  

import System.Exit ( exitFailure )

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
--   This is fail safe. If a entry can not be found of the part after a dollar
--   can not be parsed as a number it will simply be ignored and remains unchanged.
template :: [String] -> String -> String
template substs ('$':t) = 
  case reads t of
    ((i,tRest):_) | i >= 0 && i < length substs -> (substs !! i) ++ template substs tRest
    _ -> '$' : template substs t
template substs (c:t) = c : template substs t
template _ [] = []