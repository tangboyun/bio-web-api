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
       (
         queryMicroTV4
       , queryMicroTV4_impl
       )
       where
import           Bio.Web.Internal.Patiently
import           Data.ByteString            (ByteString)
import  Data.ByteString.Char8  (pack,unpack,snoc)
import qualified Data.ByteString.Char8 as B8
import           Network.Shpider
import           Text.HTML.TagSoup
import Bio.Web.MicroTV4.Types
import Bio.Web.MicroTV4.Parser
import Data.Attoparsec.Char8 hiding (takeWhile,take)
import Data.Maybe
import Control.Applicative
import Data.List
import Data.List.Split
import qualified Data.Map as M


-- testOneResultURL = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&descr=&genes=ENSG00000149948&mirnas=hsa-let-7a&threshold=0.3"
-- testURL = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&genes=ENSG00000149948%20ENSG00000179361&mirnas=hsa-let-7a%20hsa-let-7c%20hsa-miR-98%20hsa-miR-17-star%20&descr=&threshold=0.3"

-- | 30 targets per page
-- testMultiPageURL = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&genes=ENSG00000149948&descr=&threshold=0.3"
-- testOnlyMiRNA = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&descr=&genes=ENSG00000143258&mirnas=hsa-let-7a&threshold=0.3"
-- testInvalidGeneIDURL = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&genes=ENSG00000&descr=&threshold=0.3"
-- testInvalidmiRNAIDURL = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&descr=&mirnas=hsa-let-&threshold=0.3"
-- testNoTargetURL = "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&genes=ENSG00000149948&mirnas=hsa-miR-340&descr=&threshold=0.3"

waitTime :: Int  
waitTime = 10



queryMicroTV4 :: [ByteString] -> [ByteString] -> Double -> IO [[MicroTRecord]]
queryMicroTV4 miRNAs genes threshold = 
  forM genes $ \gs -> 
    fmap concat $ forM (splitEvery 30 miRNAs) $ \mis -> do
      let miStr = pack "&mirnas=" `B8.append` B8.intercalate (pack "%20") mis
          geStr = pack "&genes=" `B8.append` gs 
          thres = pack "&threshold=" `B8.append` B8.pack (show threshold)
          url = urlBase `B8.append` miStr `B8.append` geStr `B8.append` thres
      queryMicroTV4_impl url
  where
    urlBase = pack "http://diana.cslab.ece.ntua.gr/DianaTools/index.php?r=microtv4/results&descr="

queryMicroTV4_impl :: ByteString -> IO [MicroTRecord]
queryMicroTV4_impl url = runShpider $ do    
  (_,p) <- patiently waitTime download (unpack url)
  -- Remove noise.
  let ts' = takeWhile -- remove footer
            (\tag ->
              if isTagOpenName "div" tag
              then case tag of
                TagOpen _ (("class","footer"):_) -> False
                _ -> True
              else True) $ 
            dropWhile (\tag -> not $ tag ~== TagText "Results:") $ -- remove header
            dropWhile (\tag -> not $ tag ~== TagClose "strong") $ 
            tags p
  when (null ts') $ -- throw exception when invalid inputs
    error "Invalid GeneID or miRNA."
  let ts = dropWhile (not . isTagText) $ tail $
           dropWhile (not . isTagText) $ tail ts' 
      n  = fromJust $ 
           maybeResult $ -- throw error if page style has been changed.
           parse (decimal :: Parser Int) $ 
           snoc (pack $ fromTagText $ head ts) ' ' -- padding a space to avoid extracting partial result
  case n of
    0    -> return [] -- 0 records
    _    -> do
      let (ts1,ts2) = 
            break
            ( ~== TagText "Also Predicted") ts
          mi_dis = map ((\e -> 
                          if null e
                          then Nothing 
                          else Just $ 
                               map
                               ((\(TagOpen _ (("href",url'):[]):TagText dname:[]) -> 
                                  Di (pack dname) (extractPUBMEDID $ pack url')) . take 2) e) . 
                        tail .  -- get links with diseases name
                        groupBy (\_ b -> b ~/= TagOpen "a" []) . 
                        tail . dropWhile isTagText . 
                        fst . head . dropWhile 
                        (not . 
                         ([TagClose "table" -- TAG List indicate the end of each miRNA info
                          ,TagClose "td"
                          ,TagClose "tr"
                          ,TagClose "table"] `isPrefixOf`) . snd) . 
                        (\ts_ -> zip (inits ts_) (tails ts_))) $ 
                   tail $ splitOn [TagText "Related diseases:"] ts1
          cop_is = map 
                   ((\(a:b:c:[]) -> CoP a b c) . 
                    map 
                    (\(TagOpen _ ((_,img_name):_)) -> 
                      img_name /= "images/small_box_empty.png"
                    )) $ 
                    splitEvery 3 $ filter 
                    (\(TagOpen _ ((_,img_name):_)) -> 
                      "images/small_box_" `isPrefixOf` img_name) $ 
                    filter (~== TagOpen "img" []) ts2        
          str1 = B8.unwords $ map (pack . fromTagText) $ 
                 filter isTagText ts1
          str2 = B8.unwords $ map (pack . fromTagText) $ 
                 filter isTagText ts2
          (mis,gis) = fromJust $ maybeResult $ -- Input both miRNA & Gene or DIE
                      flip parse str1 $
                      manyTill anyChar 
                      (try $ string (pack "Threshold is set to") *> 
                       skipSpace *> 
                       double *> skipSpace *> char '.' *> skipSpace) *>
                      parseRIsAndGIs
          ris =  map (\(RI_impl a b c d,di) -> RI a b c d di) $ 
                 zip mis mi_dis
          riMap = M.fromList $ 
                  zip (zipWith ($) (repeat miRBaseID) ris) ris       
          giMap = M.fromList $        
                  zip (zipWith ($) (repeat ensgID) gis) gis
          mr_impls = fromJust $ maybeResult $ 
                     parse (string (pack "Also Predicted") *> parseMRs) str2
      return $! map 
        (\(MR eid _ mi' a b c d,cop) ->  
          MTR (giMap M.! eid) (riMap M.! mi') a b c cop d) $ 
        zip mr_impls cop_is
