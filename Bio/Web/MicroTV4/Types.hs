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

module Bio.Web.MicroTV4.Types
       
       where

import Data.ByteString (ByteString)


data GeneInfo = GI {
   ensgID :: !ByteString
  ,geneName :: !ByteString
  ,refSeqID :: ![ByteString]
  ,description :: !ByteString
  ,keggPathways :: !(Maybe [ByteString])
  ,chromosome :: !ByteString -- X,1,2,3,4 ...
  } deriving (Show)
                
data MiRNA_impl = RI_impl {
   miID :: !ByteString
  ,miAccession :: !ByteString
  ,related :: !(Maybe ByteString)
  ,miSeq :: !ByteString
  } deriving (Show)


data MiRNAInfo = RI {
   miRBaseID :: !ByteString
  ,miRBaseAccession :: !ByteString
  ,relatedNames :: !(Maybe ByteString)
  ,miRNASeq :: !ByteString
  ,relatedDiseases :: !(Maybe [Disease])
  } deriving (Show)

data Disease = Di {
   diseaseName :: !ByteString
  ,references :: ![ByteString] -- pubmed id
  } deriving (Show)

data BindingSite = BS {
   bindingType :: !ByteString
  ,posAtUTR3' :: !(Int,Int)
  ,bindSeq :: !ByteString
  ,seedPosAtUTR3' :: !(Int,Int)
  ,seedSeq :: !ByteString
  ,score :: {-# UNPACK #-} !Double
  ,conservation :: {-# UNPACK #-} !Int
  ,posOnChromosome :: !PosOnCh
  } deriving (Show)
                   
data PosOnCh = P {
   chrome :: !ByteString
  ,beg :: ![Int]
  ,end :: ![Int]
  } deriving (Show)
               
data CoP = CoP !Bool 
           !Bool 
           !Bool
           deriving (Show)
                    
data MR_impl = MR {                    
   ensg :: !ByteString
  ,name :: !ByteString
  ,mi :: !ByteString
  ,miT :: {-# UNPACK #-} !Double
  ,sig :: {-# UNPACK #-} !Double
  ,pre :: {-# UNPACK #-} !Double
  ,bs :: ![BindingSite]
  } deriving (Show)
  
               
               
data MicroTRecord = MTR {
   geneInfo :: GeneInfo
  ,miRNAInfo :: MiRNAInfo
--   ensgID :: !ByteString
--  ,geneName :: !ByteString 
--  ,miRNA :: !ByteString
  ,miTGScore :: {-# UNPACK #-} !Double
  ,signalNoiseRatio :: {-# UNPACK #-} !Double
  ,precision :: {-# UNPACK #-} !Double
  ,coPredictedInfo :: !CoP
  ,bindingSites :: ![BindingSite]
  } deriving Show
                   
