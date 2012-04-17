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

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack,unpack)
import Network.Shpider
import Bio.Web.DBFetch.Types
import Bio.Sequence.GB.Parser
import Bio.Sequence.GB.Types hiding (source)
import Data.Attoparsec.ByteString.Char8
import Control.Applicative
import Bio.Web.Internal.Patiently
import Data.Maybe
import Data.List
import qualified Data.Map as M

waitTime :: Int
waitTime = 10

defaultPara :: Para
defaultPara = Para RefSeqN FORMAT_Default

parseDB :: Parser (Maybe [GBRecord])
parseDB = (fmap (Just) parseGBs) <|> (string "ERROR 12 No entries found." *> return Nothing)

seqFetch :: Para -> [ByteString] -> IO [Maybe SeqRecord]
seqFetch (Para db format) geneIds  = runShpider $ do
  (_,p) <- patiently waitTime download url
  let result = eitherResult $ flip feed "" $ parse parseDB $ pack $ source p
  case result of
    Left str -> error $ "ERROR: " ++ str
    Right Nothing -> return $ replicate (length geneIds) Nothing
    Right (Just seqs) -> do
      let m = M.fromList $ zip (map (locusName . locus) seqs) seqs
      return $ map (fmap SR . (`M.lookup` m)) geneIds    
  where
    baseURL = "http://www.ebi.ac.uk/Tools/dbfetch/dbfetch?"
    url = baseURL ++ "db=" ++ toPara db ++ 
          "&style=" ++ toPara Raw ++ "&id=" ++ (
          intercalate "," $ map unpack geneIds)

seqFetch' (Para db format) geneIds  = runShpider $ do
  (_,p) <- patiently waitTime download url
  return $ parse parseGBs $ pack $ source p
  where
    baseURL = "http://www.ebi.ac.uk/Tools/dbfetch/dbfetch?"
    url = baseURL ++ "db=" ++ toPara db ++ 
          "&style=" ++ toPara Raw ++ "&id=" ++ (
          intercalate "," $ map unpack geneIds)
