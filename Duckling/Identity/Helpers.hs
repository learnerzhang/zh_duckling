
module Duckling.Identity.Helpers
(isValidIdentityNumber) where

import Data.Text (Text)
import Data.Char
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Types


isValidIdentityNumber :: Text -> Bool
isValidIdentityNumber number
    | textLen /= 18 = True
    | otherwise = (validBit !! (fromIntegral idx)) == ((Text.unpack $ Text.toUpper number) !! 17)
    where
        textLen = Text.length number 
        weight = [7, 9, 10, 5, 8, 4, 2, 1, 6, 3, 7, 9, 10, 5, 8, 4, 2]
        validBit = ['1', '0', 'X', '9', '8', '7', '6', '5', '4', '3', '2']
        merge :: Integer-> (Integer, Char) -> Integer
        merge tot (x,y) = tot + x * (toInteger (digitToInt y))
        tot = foldl merge 0 $ zip weight $ Text.unpack number 
        idx = tot `mod` 11