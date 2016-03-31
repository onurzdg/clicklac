
module Data.String.Extra where

import Data.Char (toLower)
import Data.String (IsString(..))       

-- Removes n characters from left and lower cases the first letter       
dropL1 :: Int -> String -> String
dropL1 n str =         
  let (fLetter, rest) = splitAt 1 . drop n $ str
  in  (map toLower fLetter) ++ rest

unlines' :: (IsString a) => [String] -> a
unlines' = fromString . unlines            
