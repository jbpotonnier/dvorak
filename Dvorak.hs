{-# LANGUAGE OverloadedStrings #-}

module Dvorak where

import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Data.List (sort)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeLatin1, encodeUtf8)
import System.Random.Shuffle (shuffleM)

type Word = Text
type Keys = Text
type Dict = [Text]

rows :: [Keys]
rows = ["aoeudhtns", "pyfgcrl", "qjkxbmwvz", "àâoôéèêëûüiïç"]

score :: Keys -> Word -> Rational
score row word = count / (toRational . LT.length $ word)
  where
    count = toRational . LT.length . LT.filter (\ c -> LT.singleton c `LT.isInfixOf` row) $ word

takeBestWords :: Dict -> Int -> Keys -> [Word]
takeBestWords dict nb row = take nb . 
                            map snd . 
                            sort . 
                            map (negate . score row &&& id) $ dict

randomWords :: Dict -> Int -> Int -> Keys -> IO [Word]
randomWords dict nbKeep nbBest row =
  take nbKeep <$> shuffleM bestWords
  where
    bestWords = takeBestWords dict nbBest row 

keys :: [Int] -> Keys
keys = LT.concat . map (rows !!)

main :: IO ()
main = do
  dictfile:nbwords:indices <- getArgs
  dict <- LT.lines . decodeLatin1 <$> LBS.readFile dictfile
  rowSet <- randomWords dict (read nbwords) 400 (keys (map read indices))
  C.putStrLn $ encodeUtf8 . LT.unwords $ rowSet
