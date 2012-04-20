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
import           Data.Attoparsec.Char8 hiding (take,takeWhile)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as B8
import           Data.List.Split
import Data.Maybe
import Data.Char (isAlpha)

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
  pos@(a,_) <- parsePos <* skipSpace
  sc <- double <* skipSpace
  con <- decimal <* skipSpace
  pch <- parsePCH <* skipSpace 
  (s1',s2') <- parseSeedBindingSite
  let bSeq = B8.pack $ filter isAlpha $ 
             map (\t ->
                   case t of
                     ('_','_') -> '_'
                     (c1,'_') -> c1
                     ('_',c2) -> c2
                     _ -> error "Invalid char.") $ 
             B8.zip s1' s2'
      seed = B8.reverse $ B8.takeWhile isAlpha $ B8.dropWhile (not . isAlpha) $ B8.reverse s2'
      lenS = B8.length seed
      i = myFindSeedIdx seed bSeq 0
  return $ BS bt pos bSeq (a+i,a+i+lenS-1) seed sc con pch
  where 
    myFindSeedIdx x y acc = let (h,t) = B8.breakSubstring x y
                                lh = B8.length h
                                y' =  B8.drop (B8.length x) t
                            in if B8.null $ snd $ B8.breakSubstring x y'
                               then acc + lh
                               else myFindSeedIdx x y' (acc + lh + B8.length x)
         
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
  
parseSeedBindingSite :: Parser (ByteString,ByteString)
parseSeedBindingSite = do
  s1 <- string "Conserved species:" *> 
        manyTill anyChar (try $ string "(3' UTR)") *> 
        skipSpace *> string "5'" *>
        manyTill (skipSpace *> anyChar) (try $ skipSpace *> string "3'") 
      
       
  s2' <- manyTill anyChar (try $ string "(miRNA)") 
  () <$ skipSpace *> string "3'" *> manyTill anyChar (try $ string "5'")
  let l = length s1
      s2 = take l $ filter (`elem` "AUGC_") $ s2'
  return (B8.pack s1,B8.pack s2)
