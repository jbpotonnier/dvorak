{-# LANGUAGE OverloadedStrings #-}

module Dvorak where

import System.Environment (getArgs)
import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeLatin1, encodeUtf8)
import System.Random.Shuffle (shuffleM)

type Word = Text
type Row = Text
type Dict = [Text]

rows :: [Row]
rows = ["àâaoôeéèêëuûüiïdhtns", "pyfgcçrl", "qjkxbmwvz"]

score :: Row -> Word -> Rational
score row word = count / (toRational. LT.length $ word)
  where
    count = toRational . LT.length . LT.filter (\ c -> (LT.singleton c) `LT.isInfixOf` row) $ word

takeBestWords :: Dict -> Int -> Row -> [Word]
takeBestWords dict nb row = take nb . map fst . sortBy (comparing $ negate . snd) . map (id &&& score row) $ dict

randomWords :: Dict -> Int -> Int ->  Row -> IO [Word]
randomWords dict nbKeep nbBest row = 
  take nbKeep <$> shuffleM bestWords
  where
    bestWords = takeBestWords dict nbBest row 

main :: IO ()
main = do
  [dictfile, nbwords, row] <- getArgs
  dict <- LT.lines . decodeLatin1 <$> LBS.readFile dictfile
  rowWords <- randomWords dict (read nbwords) 400 (rows !! (read row))
  C.putStrLn $ encodeUtf8 . LT.unwords $ rowWords
