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
module Bio.Web.GProfiler.GConvert
       (
         defaultPara
       , queryGConvert
       )
       where
import Bio.Web.GProfiler.Types
import Bio.Web.Internal.Patiently
import Data.ByteString            (ByteString)
import Data.ByteString.Char8      (pack,unpack)
import Data.Char
import Data.List.Split
import Network.Shpider
import Text.HTML.TagSoup

-- | Default: change human GeneID from RefSeq to ENSG
defaultPara :: ConvertPara
defaultPara = CPara Human ENSG Prefix_PUBMED False

-- | Convert a list of GeneID to another alias.
queryGConvert :: ConvertPara -> ByteString -> IO [GeneRecord]
queryGConvert para text = runShpider $ do
  patiently waitTime download gCovertUrl
  f:_ <- currentForms
  (_,p1) <- patiently waitTime sendForm $ myForm f para
  return $ pageToGeneRecord p1
  where
    gCovertUrl = "http://biit.cs.ut.ee/gprofiler/gconvert.cgi"
    waitTime = 30
    myForm form (CPara organ target prefix useRegion) = 
      let region = [("region_query", "on") | useRegion]
      in fillOutForm form $ (++) region $ pairs $ do
        "query"    =: unpack text -- testGeneID
        "organism" =: toPara organ  -- Organism
        "target"   =: toPara target -- Target database
        "output"   =: "txt"         -- Output type: html,txt,gem,xls,mini
        "prefix"   =: toPara prefix -- Numeric IDs treated as
    pageToGeneRecord =  map ((\(_:ori_id:_:new_id:name:descr:ns:[]) -> 
                               GR (pack ori_id) (pack new_id) (pack name)
                               (pack descr) (pack ns)
                             ) . splitOn "\t") . 
                        filter (\e -> not (all isSpace e) || not (null e)) . 
                        lines . concatMap fromTagText .
                        filter isTagText .
                        takeWhile (not . ("pre" `isTagCloseName`)) .
                        dropWhile (not . ("pre" `isTagOpenName`)) . tags

