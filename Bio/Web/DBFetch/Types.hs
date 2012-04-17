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
module Bio.Web.DBFetch.Types
       
       where
import Data.Char
import Bio.Sequence.GB.Types


data Para = Para DB Format
data SeqRecord = SR GBRecord
               | Other
                 deriving (Show)
                 
            
data DB = EDAM
        | EMBL
        | EMBLCDS
        | EMBLCON
        | EMBLCONEXP
        | EMBLSVA   -- ^ EMBL-SVA
        | EnsemblGene
        | EnsemblGenomesGene
        | EnsemblGenomesTranscript
        | EnsemblTranscript
        | EPO_Prt   -- ^ EPO Proteins
        | GenomeReviews
        | GenomeReviewsGene
        | GenomeReviewsTranscript
        | HGNC
        | HGVBase
        | IMGTHLA
        | IMGTLIGM
        | InterPro
        | IPDKIR    -- ^ IPD-KIR
        | IPDMHC    -- ^ IPD-MHC
        | IPI
        | IPIHistory
        | IPRMC  
        | IPRMCUniParc
        | JPO_Prt   -- ^ JPO Proteins
        | KIPO_Prt  -- ^ KIPO Proteins
        | LiveLists
        | MEDLINE 
        | NRNL1     -- ^ Patent DNA NRL1
        | NRNL2     -- ^ Patent DNA NRL2
        | NRPL1     -- ^ Patent Protein NRL1      
        | NRPL2     -- ^ Patent Protein NRL2
        | PDB
        | RefSeqN   -- ^ RefSeq (nucleotide)
        | RefSeqP   -- ^ RefSeq (protein)
        | RESID
        | SGT
        | Taxonomy
        | TraceArchive
        | UniParc
        | UniProtKB
        | UniRef100
        | UniRef50
        | UniRef90
        | UniSave
        | USPTO_Prt -- ^ USPTO Proteins
          deriving (Show,Eq)

data Format = FORMAT_Default
            | FORMAT_EMBL
            | FORMAT_EMBLXml12
            | FORMAT_Fasta
            | FORMAT_Annot
            | FORMAT_EMBLXml
            | FORMAT_Entrysize
            | FORMAT_InsdXml
            | FORMAT_SeqXml
              deriving (Show,Eq)

data Style = Raw
             deriving (Show)

class (Show a) => Input a where
  toPara :: a -> String
  toPara = map toLower . show
  
instance Input Style
instance Input DB
instance Input Format where
  toPara format =
    case format of
      FORMAT_EMBLXml12 -> "emblxml-1.2"
      _                -> map toLower $ drop 7 $ show format
