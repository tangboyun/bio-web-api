{-# LANGUAGE BangPatterns,CPP #-}
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

module Bio.Web.RNA22 
       (
         Para(..)
       , Binding(..)
       , RNA22Record(..)
       , Polymer(..)
       , Struct(..)
       , queryRNA22
       , defaultPara
       )
       where

import           Bio.Sequence.SeqData
import           Bio.Web.Internal.Patiently
import           Data.ByteString            (ByteString)
import qualified Data.ByteString.Char8      as B8
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           Data.Text                  (Text,pack,unpack)
import qualified Data.Text                  as T
import           Network.Shpider
import           Text.HTML.TagSoup
import           Text.StringLike

data Para = Para {
   mismatch :: {-# UNPACK #-} !Int -- ^ Maximum number of allowed UN-paired bases in seed
  ,seed :: {-# UNPACK #-} !Int   -- ^ Number of base in seed
  ,match :: {-# UNPACK #-} !Int    -- ^ Minimum number of paired-up bases in heteroduplex
  ,free :: {-# UNPACK #-} ! Int  -- ^ Maximum folding energy for heteroduplex (Kcal/mol)
  } deriving (Eq,Ord)

data Binding = Binding {
   position :: !(Int,Int)
  ,energy  :: {-# UNPACK #-} !Double
  ,polymer :: !Polymer
  ,structure :: !Struct
  } deriving (Eq,Ord)
data RNA22Record = RNA22Record {
   utrSeqid :: !ByteString 
  ,miRNAid :: !ByteString 
  ,rna22Para :: !Para 
  ,bindingSites :: !(Maybe [Binding])
  } deriving (Eq,Ord)
data Polymer = Poly {
   seqWithLinker :: !(ByteString,ByteString,ByteString) 
  ,backetNotation :: !(ByteString,ByteString,ByteString)
  } deriving (Eq,Ord)
data Struct = Struct !ByteString !ByteString !ByteString
            deriving (Eq,Ord)
    

defaultPara = Para 0 6 14 (-25)

waitTime :: Int
waitTime = 30
  
queryRNA22 :: Sequence a -> Sequence a -> Para -> IO RNA22Record
queryRNA22 utrSeq' miRNASeq' para = runShpider $ do
    patiently waitTime download rna22URL
    f:_ <- currentForms
    (_,p1) <- patiently waitTime sendForm $ fillOutForm f $ 
             makeForm (unpack miRNASeq) (unpack utrSeq) para
    if notFindBindings p1
      then return $! RNA22Record utrId miRNAId (checkPara para) Nothing
      else do 
        (_,p2) <- patiently waitTime download (head $ tagsToLinks $ tags p1)
        return $ parseRecord $ 
          init $ filter (not . null) $
          tail $ splitOn "\n\n\n" $ concatMap fromTagText $ 
          filter isTagText $ tags p2
  where 
    rna22URL = "http://cbcsrv.watson.ibm.com/rna22.html"
    utrSeq = seqToText $ castToNuc utrSeq'
    miRNASeq = seqToText $ castToNuc miRNASeq'
    toText = pack . toStr
    seqToText seq = pack ">" `B8.append` 
                    toText (seqlabel seq) `B8.append` 
                    pack "\n" `B8.append` 
                    toText (seqdata seq) `B8.append` pack "\n"
    notFindBindings page = let str = head $ map fromTagText $ 
                                  filter isTagText $ tags page
                           in "No targets found" `isPrefixOf` str
    tagsToLinks ts =
      concatMap (\(TagOpen _ xs ) ->  
                  map snd $ filter (\(a,_) -> a == "src") xs
                ) $ filter (~== TagOpen "FRAME" []) ts
    makeForm !mi !utr !para =
      let para' = checkPara para
      in [("miRNASeq",mi)
         ,("utrSeq",utr)
         ,("mismatch",show $ mismatch para')
         ,("window",show $ seed para')
         ,("match",show $ match para')
         ,("energy",show $ free para')]
    checkPara !(Para mis s m en) =
      let mis'
            | mis >= 0 && mis <= 2 = mis
            | mis < 0 = 0
            | otherwise = 2
          s'
            | s == 6 || s == 7 = s
            | s < 6 = 6
            | otherwise = 7
          m'
            | m `elem` [10 .. 19] = m
            | m < 10 = 10
            | otherwise = 19
          en'
            | en `elem` ((-15) : (-50) : [(-40) .. (-18)]) = en
            | en > (-18) = (-18)
            | otherwise = -40
      in Para mis' s' m' en'
    f =  head . words . tail . head . lines     
    miRNAId = pack $ f $ unpack miRNASeq 
    utrId = pack $ f $ unpack utrSeq
    parseRecord ss =
      let bs = sort $ map parseBindingSite ss
          p = checkPara para
      in RNA22Record utrId miRNAId p (Just bs)
    parseBindingSite str = 
      let (loc:_:total:dot:_):(m:bar:mi:_):_ = 
            filter (not . null) $ 
            map (filter (\l -> 
                          not (null l) || 
                          not (all isSpace l)) . lines) $ 
            splitOn "\n\n" str
          (beg,end,en) = let ls = words loc
                         in (read $ ls !! 2, read $ ls !! 4, read $ ls !! 9)
          m' = filter isAlpha m
          mi' = filter isAlpha mi
          f l1 l2 str = let (str1,str') = splitAt l1 str
                            (str2,str3) = splitAt (length str - l1 - l2) str'
                        in (pack str1,pack str2,pack str3)
          poly = Poly (f (length m') (length mi') total)
                 (f (length m') (length mi') dot)
          struct = Struct (pack m) (pack bar) (pack mi)
      in Binding (beg,end) en poly struct
                            
instance Show RNA22Record where
  show (RNA22Record mRNAid miRNAid (Para mi s m f) bsInfo) = 
    "Targets predicted in " ++ unpack mRNAid ++ 
    " for " ++ unpack miRNAid ++ ": \n\n" ++
    "Maximum number of allowed UN-paired bases " ++ show mi ++ 
    " in seed/nucleus of " ++ show s ++  " nucleotides.\n" ++
    "Minimum number of paired-up bases in heteroduplex: " ++ 
    show m ++ "\n" ++
    "Maximum folding energy for heteroduplex (Kcal/mol): " ++ 
    show f ++ "\n\n\n" ++ bindingSiteInfo
    where
      bindingSiteInfo = case bsInfo of
        Nothing -> "No targets found for the above thresholds.\n"
        Just bs -> show (length bs) ++ 
                   " targets found for the above thresholds.\n\n\n" ++ 
                   intercalate "\n\n" 
                   (map (\(e,i) -> show i ++ ". " ++ show e) $ 
                    zip bs [1..])
instance Show Binding where
  show (Binding (p1,p2) en poly struct) =
    "From offset " ++ show p1 ++ " to " ++ 
    show p2 ++ " | Folding energy = " ++
    show en ++ " Kcal/mol\n" ++ 
    show poly ++ "\n\n" ++ show struct ++ "\n"
instance Show Polymer where
  show (Poly (a,b,c) (e,f,g)) = indicateLine ++
    unpack a ++ seprater ++ unpack b ++ seprater ++ unpack c ++ "\n" ++
    unpack e ++ seprater ++ unpack f ++ seprater ++ unpack g
    where
      indicateLine = let len1 = B8.length a
                         len2 = B8.length b
                         len3 = B8.length c
                         id1 = "5'<-- target -->3'"
                         id2 = "-linker-"
                         id3 = "5'<-- microRNA -->3'"
                         id1' = id1 ++ replicate (len1 - length id1) ' '
                         id3' = replicate (len3 - length id3) ' ' ++ id3
                         id2' = id2 ++ replicate (len2 - length id2) ' '
                     in id1' ++ seprater ++ id2' ++ seprater ++ id3' ++ "\n"
      seprater = "  "
instance Show Struct where
  show (Struct h i j) = "2D-Structure: \n" ++
    unpack h ++ "\n" ++ unpack i ++ "\n" ++ unpack j

