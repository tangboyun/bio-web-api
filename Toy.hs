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

module Main where

import           Bio.Sequence.GB.Types
import Bio.Web.DBFetch.Types
import           Bio.Web.DBFetch
import qualified Bio.Web.DBFetch            as DB
import qualified Bio.Web.GProfiler.GConvert as GC
import           Bio.Web.GProfiler.Types
import           Bio.Web.MicroTV4
import           Bio.Web.MicroTV4.Types
import           Control.Monad
import qualified Data.ByteString.Char8      as B8
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           System.Environment
import Data.Char
import System.IO
import Text.Printf

main :: IO ()
main = do 
  (miRNAFile:geneFile:outFile:_) <- getArgs
  miStr <- B8.readFile miRNAFile
  geStr <- B8.readFile geneFile
  hOut <- openFile outFile WriteMode
  let miRNAs = map (B8.filter (not . isSpace)) $ filter (\e -> (not $ B8.null e) || all isSpace (B8.unpack e)) $ B8.lines miStr
      n = length miRNAs 
      geness = splitEvery 1 $ map (B8.filter (not . isSpace)) $  B8.lines geStr
  putStrLn "#RefSeqID\tENSG           \tGene\tMuTaME    \tKEGG"
  hPutStrLn hOut "#RefSeqID\tENSG           \tGene\tMuTaME    \tKEGG"
  hFlush hOut
  forM_ geness $ \genes -> do    
    srs <- seqFetch DB.defaultPara genes
--    mapM_ print genes
    ensgs <- fmap (map (\e -> (convertedAlias e,geneSymbol e))) $ GC.queryGConvert GC.defaultPara (B8.unwords genes)
--    mapM_ print ensgs
--    mapM_ print srs
    mrs <- queryMicroTV4 miRNAs (fst $ unzip ensgs) 0.3
--    print mrs
    
    let bsss = map (\e -> case e of
                          Just e' -> Just $ map (map posAtUTR3' . bindingSites) e'
                          Nothing   -> Nothing) mrs
        ks = concatMap (\e -> case e of 
                     Just [] -> [Nothing]
                     Just e' -> [keggPathways $ geneInfo $ head e']
                     Nothing -> [Nothing]) mrs       
    forM_ (zip5 genes srs bsss ensgs ks) $ \(g,srSeq,bss,(ensg,name),k) -> do
      case srSeq of
        Just (SR seq) -> do
          let ref_id = locusName $ locus seq
              kstr = B8.unpack $ B8.intercalate (B8.pack ";") $ fromMaybe [B8.pack "N/A"] k  
          case bss of 
            Just bss' -> do      
              let score = mutameScore (extractUTRLength3' seq) n bss'
              printf "%s\t%s\t%s\t%.6f\t%s\n" (B8.unpack ref_id) (B8.unpack ensg) (B8.unpack name) score kstr
              hPrintf hOut "%s\t%s\t%s\t%.6f\t%s\n" (B8.unpack ref_id) (B8.unpack ensg) (B8.unpack name) score kstr
              hFlush hOut
            Nothing -> do
              printf "%s\t%s\t%s\t%s\t%s\n" (B8.unpack ref_id) (B8.unpack ensg) (B8.unpack name) ("N/A" :: String) kstr
              hPrintf hOut "%s\t%s\t%s\t%s\t%s\n" (B8.unpack ref_id) (B8.unpack ensg) (B8.unpack name) ("N/A" :: String) kstr
              hFlush hOut
        Nothing -> do  
          printf "%s\t%s\n" (B8.unpack g) ("Not founded by DBFetch." :: String)
          hPrintf hOut "%s\t%s\n" (B8.unpack g) ("Not founded by DBFetch." :: String)
          hFlush hOut
extractUTRLength3' seq = 
  let id = locusName $ locus seq
      tp = molType $ moleculeType $ locus seq
      fs = features seq
      cdss = filter ((== "CDS") . feature) fs
      utrs = filter ((== "3'UTR") . feature) fs
      exons = filter ((== "exon") . feature) fs
  in if tp /= "mRNA"
     then error "ERROR: can't extract 3'UTR"
     else  if length utrs == 1 -- have 3'UTR feature
           then let (a,b) = parseLoc $ locationDescriptor $ head utrs
                in b - a
           else case cdss of
             [cds] -> case exons of
               [] -> let l = sequenceLength $ locus seq
                         (a,b) = parseLoc $ locationDescriptor cds
                     in l - b
                 
               _ -> let (a,b) = parseLoc $ locationDescriptor cds
                   in sum $ map (\e -> snd e - fst e) $ (\((p1,p2):xs) -> (b,p2):xs) $                
                      dropWhile (\(p1,p2) -> not (b >= p1 && b <= p2)) $ 
                      sort $ map (parseLoc.locationDescriptor) exons
             _ -> error "None or Multiple cds founded."  
  where 
    parseLoc str = let a:b:[] = splitOn ".." $ B8.unpack str
                   in (fst $ fromJust $ B8.readInt $ B8.filter (not . isSpace) $ B8.pack a,
                       fst $ fromJust $ B8.readInt $ B8.filter (not . isSpace) $ B8.pack b)
                       
             
mutameScore :: Int -> Int -> [[(Int,Int)]] -> Double
mutameScore len nmiR bss =
  if null bss'
  then 0
  else c1 * c2 * c3 * c4
  where
    bss' = map sort $ filter (not . null) bss
    nRNA = length bss'
    totalN = sum nForEach
    nForEach = map (fromIntegral . length) bss'
    c1 = (fromIntegral nRNA) / fromIntegral nmiR
    c2 = sum $ zipWith (/) nForEach $ map f1 bss'       -- unclear
    c3 = sum $ zipWith (/) (map ((^^2) . f1) bss') (map f2 bss')  -- unclear 
    c4 = (totalN - (fromIntegral nRNA) + 1) / totalN
    f1 ((a,b):[]) = let l = (fromIntegral (a + b)) / 2
                        leng = fromIntegral len
                    in max (leng-l) l
    f1 xs = let (a1,b1) = head xs
                (a2,b2) = last xs
            in (fromIntegral (a2+b2)) / 2 - 
               (fromIntegral (a1+b1)) / 2
    f2 ((a,b):[]) = let l = (fromIntegral (a + b)) / 2
                        leng = fromIntegral len
                    in l ^^ 2 + (leng - l) ^^ 2
    f2 (x:xs) = fst $ foldl' (\(acc,(a1,b1)) (a2,b2) ->
                                let l1 = fromIntegral (a1+b1) / 2
                                    l2 = fromIntegral (a2+b2) / 2
                                    acc' = acc + (l2 - l1) ^^ 2
                                in (acc',(a2,b2))
                             ) (0,x) xs
