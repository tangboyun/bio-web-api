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
  ,keggPathways :: !(Maybe ByteString)
  ,chromosome :: {-# UNPACK #-} !Int
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
  ,score :: {-# UNPACK #-} !Double
  ,conservation :: {-# UNPACK #-} !Int
  ,posOnChromosome :: !PosOnCh
  } deriving (Show)
                   
data PosOnCh = P {
   chrome :: {-# UNPACK #-} !Int
  ,beg :: {-# UNPACK #-} !Int
  ,end :: {-# UNPACK #-} !Int
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
                   
