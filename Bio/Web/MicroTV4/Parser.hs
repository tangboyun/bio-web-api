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

module Bio.Web.MicroTV4.Parser 
       
       where

import           Bio.Web.MicroTV4.Types
import           Control.Applicative
import           Data.Attoparsec.Char8
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B8
import           Data.List.Split
import Data.Maybe

-- http://www.ncbi.nlm.nih.gov/sites/entrez?db=Pubmed&term=17308078[uid]%20OR%2018413726[uid]%20OR%2018413726[uid]%20OR%2017308078[uid]%20OR%2020068076[uid]%20OR%2020797623[uid]%20OR%2016530703[uid]%20OR%2019825969[uid]
extractPUBMEDID :: ByteString -> [ByteString]
extractPUBMEDID = fromJust . maybeResult . flip feed "" . parse parseNCBILink
  
parseNCBILink :: Parser [ByteString]
parseNCBILink = do
  str <- string "http://www.ncbi.nlm.nih.gov/sites/entrez?db=Pubmed&term=" *> takeByteString 
  return $! map (f . B8.pack) $ splitOn "%20OR%20" $ B8.unpack str
  where f = \e -> foldr ($) e $ replicate 5 B8.init -- remove tailing "[uid]"

parseRIsAndGIs :: Parser ([MiRNA_impl],[GeneInfo])
parseRIsAndGIs = do
  ris <- many1 $ parseRI <* skipSpace
  gis <- many1 $ parseGI <* skipSpace
  return (ris,gis)
  
parseGI :: Parser GeneInfo
parseGI = do
  string "Ensembl Gene ID:" *> skipSpace
  enId <- takeWhile1 (not . isSpace) <* skipSpace <* 
          char '\160' <* skipSpace -- damn &nbsp; can't be parsed as ' '
  string "Gene Name:" *> skipSpace
  ge <- takeWhile1 (not . isSpace) <* skipSpace
  string "Refseq IDs:" *> skipSpace
  refStr <- manyTill anyChar (try $ string " Description:") <* skipSpace
  des <- manyTill anyChar (try $ string " External links:") <* skipSpace
  manyTill anyChar (try $ string "Kegg pathways:") <* skipSpace
  kegg <- fmap (\_ -> Nothing)
          (string "No related KEGG pathways." *> 
           skipSpace *> string "Chromosome:" *> skipSpace) <|>
          fmap (Just . B8.pack) (manyTill anyChar $ try $ 
                     skipSpace *> string "Chromosome:" <* skipSpace)
  ch <- decimal
  return $ GI enId ge (map B8.pack $ splitOn ", " refStr) (B8.pack des)
              kegg ch
    
  
parseRI :: Parser MiRNA_impl
parseRI = do
  string "Name:" *> skipSpace 
  mi' <- takeWhile1 (not . isSpace) <* skipSpace <* char '\160' <* skipSpace
  string "Alternative description:" *> skipSpace
  miAcc <- takeWhile1 (not . isSpace) <* skipSpace
  string "Related names:" *> skipSpace
  reName <- fmap (\_ -> Nothing) 
            (string "There are no related names for this entry." *> 
             skipSpace ) <|>
             fmap Just (takeWhile1 (not . isSpace) <* skipSpace)
  string "miRNA sequence:" *> skipSpace
  miS <- takeWhile1 (not . isSpace) <* skipSpace
  string "External links:" *> 
    skipSpace *> string "miRBase" *>
    skipSpace *> string "Related diseases:" *> 
    many1 (satisfy (/= '\n'))
  return $ RI_impl mi' miAcc reName miS
  
parseMRs :: Parser [MR_impl]
parseMRs = do
  many1 $ skipSpace *> parseMR_impl 
  
parseMR_impl :: Parser MR_impl
parseMR_impl = do
  skipSpace *> (decimal :: Parser Integer) *> skipSpace 
  ensg' <- parseENSGID <* skipSpace
  gn' <- parseGeneSymb <* skipSpace
  mi' <- takeWhile1 (not . isSpace) <* skipSpace
  miTG' <- double <* skipSpace
  snr' <- double <* skipSpace
  pre' <- double <* skipSpace
  manyTill anyChar (try $ string "Binding Type 3' UTR position Score Conservation")
  bss <- many1 $ skipSpace *> parseBindingSite 
  return $ MR ensg' gn' mi' miTG' snr' pre' bss
  
parseBindingSite :: Parser BindingSite  
parseBindingSite = do
  bt <- parseBT <* skipSpace
  pos <- parsePos <* skipSpace
  sc <- double <* skipSpace
  con <- decimal <* skipSpace
  pch <- parsePCH <* skipSpace <* parseGarbage
  return $ BS bt pos sc con pch
         
parseGeneSymb :: Parser ByteString  
parseGeneSymb = 
  char '(' *> (fmap B8.pack $ manyTill anyChar (try $ char ')'))
  
parseENSGID :: Parser ByteString
parseENSGID = do
  str <- string "ENSG"
  num <- takeWhile1 isDigit
  return $! str `B8.append` num
   
parseBT :: Parser ByteString 
parseBT = do
  d <- many1 digit
  s <- string "mer"
  return $! (B8.pack d) `B8.append` s

parsePos :: Parser (Int,Int)
parsePos = do
  p1 <- decimal <* char '-'
  p2 <- decimal
  return (p1,p2)
  
parsePCH :: Parser PosOnCh
parsePCH = do
  string "Position on chromosome:" *> skipSpace
  d1 <- decimal <* char ':'
  d2 <- decimal <* char '-'
  d3 <- decimal 
  return $ P d1 d2 d3

parseGarbage :: Parser ()
parseGarbage = do
  string "Conserved species:" *> 
    manyTill anyChar (try $ parseTerminator) *> return ()
  where
    parseTerminator =
      string "(miRNA) 3' " *> many1 (satisfy (not . isDigit)) *> string "5'"
  
