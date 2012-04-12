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
module Bio.Web.MicroTV4 
       
       where
import           Bio.Web.Internal.Patiently
import           Data.ByteString            (ByteString)
import  Data.ByteString.Char8  (pack,unpack,snoc)
import qualified Data.ByteString.Char8 as B8
import           Network.Shpider
import           Text.HTML.TagSoup
import Bio.Web.MicroTV4.Types
import Bio.Web.MicroTV4.Parser
import Data.Attoparsec.Char8 (parse,maybeResult,decimal,Parser)
import Data.Maybe

urlBase =  "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&descr=&"

testOneResultURL = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&descr=&genes=ENSG00000149948&mirnas=hsa-let-7a&threshold=0.3"
test4ResultURL = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&genes=ENSG00000149948&mirnas=hsa-let-7a%20hsa-let-7c%20hsa-miR-98%20hsa-miR-17-star%20&descr=&threshold=0.3"

-- | 30 targets per page
testMultiPageURL = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&genes=ENSG00000149948&descr=&threshold=0.3"

testInvalidGeneIDURL = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&genes=ENSG00000&descr=&threshold=0.3"
testInvalidmiRNAIDURL = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&descr=&mirnas=hsa-let-&threshold=0.3"

testNoTargetURL = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&genes=ENSG00000149948&mirnas=hsa-miR-340&descr=&threshold=0.3"

testQuery = do 
  str <- runShpider $ do
    (_,p) <- patiently waitTime download testNoTargetURL
    return $ B8.unwords $ map (pack . fromTagText) $ filter isTagText $ removeNoise p
--    return $ fromJust $ maybeResult $ parse parseMRs $ unwords $ map (pack . fromTagText) $ filter isTagText $ tail $ removeNoise p
--  return str
  putStrLn $ unpack str
  
-- | Return [] if :
-- 1. Invalid GeneID or miRNA.
-- 2. No Target founded.
  
removeNoise :: Page -> [Tag String]
removeNoise = 
  dropWhile 
  (\tag -> 
    not $ tag ~== TagText "Also Predicted"
  ) . takeWhile 
  (\tag ->
    if isTagOpenName "div" tag
    then case tag of
      TagOpen _ (("class","footer"):_) -> False
      _ -> True
    else True) . dropWhile 
  (\tag -> 
    if isTagOpenName "form" tag
    then case tag of
      TagOpen _ (("id","changepage"):("name","changepage"):_) -> False
      _ -> True
    else True
  ) . dropWhile (\tag -> not $ tag ~== TagClose "strong") . tags
  
waitTime = 10

queryMicroTV4 :: ByteString -> IO (Maybe [MicroTRecord])
queryMicroTV4 txt = runShpider $ do    
  (_,p) <- patiently waitTime download test4ResultURL
  let ts' = takeWhile -- Remove noise.
            (\tag ->
              if isTagOpenName "div" tag
              then case tag of
                TagOpen _ (("class","footer"):_) -> False
                _ -> True
              else True) $ 
            dropWhile (\tag -> not $ tag ~== TagText "Results:") $
            dropWhile (\tag -> not $ tag ~== TagClose "strong") $ 
            tags p
  when (null ts') $ -- Invalid inputs
    error "Invalid GeneID or miRNA."
  let ts = dropWhile (not . isTagText) $ tail $
           dropWhile (not . isTagText) $ tail ts' 
      n = fromJust $ maybeResult $                -- throw error if page style has been changed.
          parse (decimal :: Parser Int) $ 
          snoc (pack $ fromTagText $ head ts) ' ' -- can't extract partial result
  case n of
    0    -> return Nothing
    _    -> do
      
      let (ts1,ts2) = 
            break
            ( ~== TagText "Also Predicted") ts -- $ dropWhile  
            -- (\tag -> 
            --   if isTagOpenName "form" tag
            --   then case tag of
            --     TagOpen _ (("id","changepage"):("name","changepage"):_) -> False
            --     _ -> True
            --   else True) ts
      liftIO $ print $ unwords $ map fromTagText $ filter isTagText $ ts1
      liftIO $ replicateM_ 10 $ print ""
      liftIO $ print $ unwords $ map fromTagText $ filter isTagText $ ts2
      undefined 
