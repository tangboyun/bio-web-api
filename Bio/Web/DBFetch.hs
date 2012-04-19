{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module : 
-- Copyright : (c) 2012 Boyun Tang
-- License : BSD-style
-- Maintainer : tangboyun@hotmail.com
-- Stability : experimental
-- Portability : ghc
--
-- 
--
-----------------------------------------------------------------------------
module Bio.Web.DBFetch
       
       
       where

import           Bio.Sequence.GB.Parser
import           Bio.Sequence.GB.Types            hiding (source)
import           Bio.Web.DBFetch.Types
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import           Data.ByteString.Char8            (pack,unpack)
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.List
import qualified Data.Map                         as M
import           Data.Maybe
import           Network.HTTP.Conduit


defaultPara :: Para
defaultPara = Para RefSeqN FORMAT_Default

parseDB :: Parser (Maybe [GBRecord])
parseDB = Just <$> parseGBs <|> Nothing <$ string "ERROR 12 No entries found."

seqFetch :: Para -> [ByteString] -> IO [Maybe SeqRecord]
seqFetch (Para db format) geneIds  = do
  request <- parseUrl url
  result <- withManager $ \manager -> do
    Response _ _ _ bsrc <- http request manager
    bsrc $$ sinkParser parseDB
  case result of
    Nothing -> return $ replicate (length geneIds) Nothing
    Just seqs -> do
      let m = M.fromList $ zip (map (locusName . locus) seqs) seqs
      return $ map (fmap SR . (`M.lookup` m)) geneIds          
  where
    baseURL = "http://www.ebi.ac.uk/Tools/dbfetch/dbfetch?"
    url = baseURL ++ "db=" ++ toPara db ++ 
          "&style=" ++ toPara Raw ++ "&id=" ++ (
          intercalate "," $ map unpack geneIds)

