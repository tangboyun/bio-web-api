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
import Data.List (foldl')
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
          char '\160' <* skipSpace <?> "ERROR: parsing ENSG id." -- damn &nbsp; can't be parsed as ' ' 
  string "Gene Name:" *> skipSpace
  ge <- takeWhile1 (not . isSpace) <* skipSpace <?> "ERROR: parsing GeneName."
  string "Refseq IDs:" *> skipSpace
  refStr <- manyTill anyChar (try $ skipSpace *> string "Description:") <* skipSpace
  des <- manyTill anyChar (try $ skipSpace *> string "External links:") <* skipSpace
  manyTill anyChar (try $ string "Kegg pathways:") <* skipSpace
  kegg <- fmap (\_ -> Nothing)
          (string "No related KEGG pathways." *> 
           skipSpace *> string "Chromosome:" *> skipSpace) <|>
          fmap (Just . map B8.pack . splitOn "\t") (manyTill anyChar $ try $ 
                     skipSpace *> string "Chromosome:" <* skipSpace)
  ch <- takeWhile1 (not . isSpace) 
  return $ GI enId ge (map B8.pack $ splitOn ", " refStr) (B8.pack des)
              kegg ch
    
  
parseRI :: Parser MiRNA_impl
parseRI = do
  string "Name:" *> skipSpace 
  mi' <- takeWhile1 (not . isSpace) <* skipSpace <* char '\160' <* skipSpace <?> "ERROR: parsing  miRNA id."
  string "Alternative description:" *> skipSpace
  miAcc <- takeWhile1 (not . isSpace) <* skipSpace <?> "ERROR: parsing miRNA accession."
  string "Related names:" *> skipSpace
  reName <- fmap (\_ -> Nothing) 
            (string "There are no related names for this entry." *> 
             skipSpace ) <|>
             fmap Just (takeWhile1 (not . isSpace) <* skipSpace)
  string "miRNA sequence:" *> skipSpace
  miS <- takeWhile1 (not . isSpace) <* skipSpace <?> "ERROR: parsing miRNA sequence."
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
  pos@(_,b) <- parsePos <* skipSpace
  sc <- double <* skipSpace
  con <- decimal <* skipSpace
  pch <- parsePCH <* skipSpace 
  str <- parseSeedBindingSite
  let (t,len) = fst $ foldl' (\a@((accT,acc),i) (p,l) -> 
                               if i && p 
                               then ((accT,l),False)
                               else if (not p) && i
                                    then ((accT + l,acc),i)
                                    else a
                             )  ((0,0),True) $ 
                map (\s -> (B8.all (== '_') s,B8.length s)) $ 
                B8.group $ B8.reverse str
  return $ BS bt pos (b-t-len,b-t) sc con pch
         
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
  s1 <- manyTill anyChar (try $ char ':')
  d2 <- decimal 
  d2s <- optional $ many1 $ char ';' *> decimal
  char '-'
  d3 <- decimal 
  d3s <- optional $ many1 $ char ';' *> decimal
  return $ P (B8.pack s1) (d2:fromMaybe [] d2s) (d3:fromMaybe [] d3s)

parseGarbage :: Parser ()
parseGarbage = do
  string "Conserved species:" *> 
    manyTill anyChar (try $ parseTerminator) *> return ()
  where
    parseTerminator =
      string "(miRNA) 3' " *> many1 (satisfy (not . isDigit)) *> string "5'"
  
parseSeedBindingSite :: Parser ByteString
parseSeedBindingSite = do
  s <- string "Conserved species:" *> 
      manyTill anyChar (try $ string "(3' UTR)") *> 
      skipSpace *> string "5'" *>
      manyTill (skipSpace *> anyChar) (try $ skipSpace *> string "3'") <* 
      manyTill anyChar (try $ parseTerminator) <?> "ERROR: parseSeedBindingSite"
  return $ B8.pack s 
  where
    parseTerminator =
      string "(miRNA)" *> skipSpace *> string "3'" *> many1 (satisfy (not . isDigit)) *> string "5'"
