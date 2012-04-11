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
module Bio.Web.GProfiler.Types 
       
       where
import Data.ByteString (ByteString)

type QueryRegion = Bool
data ConvertPara = CPara Organism TargetDB Prefix QueryRegion
data GeneRecord = GR {
   initialAlias :: ByteString 
  ,convertedAlias :: ByteString 
  ,geneSymbol :: ByteString 
  ,description :: ByteString 
  ,nameSpace :: ByteString
  } deriving (Eq,Ord,Show)

data Prefix = Prefix_AFFY_HUGENE_1_0_ST_V1
            | Prefix_MIM_GENE_ACC
            | Prefix_SHARES_CDS_WITH_ENST
            | Prefix_PUBMED
            | Prefix_HGNC_MB001_ACC
            | Prefix_HGNC_ACC
            | Prefix_WIKIGENE_ACC
            | Prefix_DBASS5_ACC
            | Prefix_ILLUMINA_HUMANWG_6_V1
            | Prefix_MIM_MORBID_ACC
            | Prefix_AFFY_HUEX_1_0_ST_V2
            | Prefix_ENTREZGENE_ACC
            | Prefix_DBASS3_ACC
              deriving (Read,Show)
                       
data TargetDB = AFFY_HC_G110
              | AFFY_HG_FOCUS
              | AFFY_HG_U133A
              | AFFY_HG_U133A_2
              | AFFY_HG_U133B
              | AFFY_HG_U133_PLUS_2
              | AFFY_HG_U95A
              | AFFY_HG_U95AV2
              | AFFY_HG_U95B
              | AFFY_HG_U95C
              | AFFY_HG_U95D
              | AFFY_HG_U95E
              | AFFY_HUEX_1_0_ST_V2
              | AFFY_HUGENEFL
              | AFFY_HUGENE_1_0_ST_V1
              | AFFY_U133_X3P
              | AGILENT_CGH_44B
              | AGILENT_WHOLEGENOME
              | CCDS
              | CCDS_ACC
              | CLONE_BASED_ENSEMBL_GENE
              | CLONE_BASED_ENSEMBL_TRANSCRIPT
              | CLONE_BASED_VEGA_GENE
              | CLONE_BASED_VEGA_TRANSCRIPT
              | CODELINK_CODELINK
              | DBASS3
              | DBASS3_ACC
              | DBASS5
              | DBASS5_ACC
              | EMBL
              | ENSG
              | ENSP
              | ENST
              | ENS_HS_GENE
              | ENS_HS_TRANSCRIPT
              | ENS_HS_TRANSLATION
              | ENS_LRG_GENE
              | ENS_LRG_TRANSCRIPT
              | ENTREZGENE
              | ENTREZGENE_ACC
              | GO
              | GOSLIM_GOA
              | HGNC
              | HGNC_ACC
              | HGNC_MB001
              | HGNC_MB001_ACC
              | HGNC_TRANSCRIPT_NAME
              | HPA
              | HPA_ACC
              | ILLUMINA_HUMANHT_12
              | ILLUMINA_HUMANWG_6_V1
              | ILLUMINA_HUMANWG_6_V2
              | ILLUMINA_HUMANWG_6_V3
              | IPI
              | IPI_ACC
              | LRG
              | MEROPS
              | MIM_GENE
              | MIM_GENE_ACC
              | MIM_MORBID
              | MIM_MORBID_ACC
              | MIRBASE
              | MIRBASE_ACC
              | MIRBASE_GENE_NAME 
              | MIRBASE_TRANSCRIPT_NAME
              | OTTG
              | OTTT
              | PDB
              | PHALANX_ONEARRAY
              | PROTEIN_ID
              | PROTEIN_ID_ACC
              | PUBMED
              | REFSEQ_DNA
              | REFSEQ_DNA_ACC
              | REFSEQ_GENOMIC
              | REFSEQ_MRNA
              | REFSEQ_MRNA_ACC
              | REFSEQ_MRNA_PREDICTED
              | REFSEQ_MRNA_PREDICTED_ACC
              | REFSEQ_NCRNA
              | REFSEQ_NCRNA_ACC
              | REFSEQ_NCRNA_PREDICTED
              | REFSEQ_NCRNA_PREDICTED_ACC
              | REFSEQ_PEPTIDE
              | REFSEQ_PEPTIDE_ACC
              | REFSEQ_PEPTIDE_PREDICTED
              | REFSEQ_PEPTIDE_PREDICTED_ACC
              | RFAM
              | RFAM_ACC
              | RFAM_TRANSCRIPT_NAME
              | SHARES_CDS_AND_UTR_WITH_OTTT
              | SHARES_CDS_WITH_ENST
              | SHARES_CDS_WITH_ENST_ACC
              | SHARES_CDS_WITH_OTTT
              | UCSC
              | UNIGENE
              | UNIPROTSPTREMBL
              | UNIPROTSPTREMBL_ACC
              | UNIPROTSWISSPROT
              | UNIPROTSWISSPROT_ACC
              | UNIPROT_GENENAME
              | VEGA_GENE
              | VEGA_GENE_ACC
              | VEGA_TRANSCRIPT
              | VEGA_TRANSCRIPT_ACC
              | VEGA_TRANSLATION
              | WIKIGENE
              | WIKIGENE_ACC
                deriving (Read,Show)
                         
class Input a where  
  toPara :: a -> String
                         
instance Input Organism where                
  toPara organ =
    case organ of
      Human -> "hsapiens"
      
instance Input TargetDB where
  toPara = show

instance Input Prefix where
  toPara = drop 7 . show

data Organism = Human --  ^ Homo sapiens
              | Mouse --  ^ Mus musculus
              | Yeast --  ^ Saccharomyces cerevisiae
              | Rat   --  ^ Rattus norvegicus
              | Fly   --  ^ Drosophila melanogaster
              | Worm  --  ^ Caenorhabditis elegans
              | Zebrafish --  ^ Danio rerio
              | Aedes --  ^ Aedes aegypti 伊蚊
              | Panda --  ^ Ailuropoda melanoleuca
              | Lizard --  ^ Anolis carolinensis
              | Anopheles  --  ^ Anopheles gambiae 按蚊
              | Bovine --  ^ Bos taurus
              | Marmoset --  ^ Callithrix jacchus 狨猴
              | Dog --  ^ Canis familiaris
              | Cavy --  ^ Cavia porcellus 豚鼠
              | Sloth --  ^ Choloepus hoffmanni 树懒
              | Tunicate --  ^ Ciona intestinalis 海鞘1
              | Ciona --  ^ Ciona savignyi 海鞘2
              | Armadillo --  ^ Dasypus novemcinctus 犰狳
              | Dipodomys --  ^ Dipodomys ordii
              | Echinops --  ^ Echinops telfairi
              | Horse --  ^ Equus caballus
              | Hedgehog --  ^ Erinaceus europaeus 刺猬
              | Cat --  ^ Felis catus
              | Chicken --  ^ Gallus gallus
              | Tiddler --  ^ Gasterosteus aculeatus 鲰
              | Gorillas --  ^ Gorilla gorilla 大猩猩
              | Elephant --  ^ Loxodonta africana 非洲象
              | Monkey --  ^ Macaca mulatta 猕猴
              | Wallaby --  ^ Macropus eugenii 袋鼠
              | Turkey  --  ^ Meleagris gallopavo 火鸡
              | Lemur   --  ^  Microcebus murinus 狐猴
              | Opossum --  ^ Monodelphis domestica 负鼠
              | Bat --  ^ Myotis lucifugus
              | Gibbon --  ^ Nomascus leucogenys 长臂猿
              | Pika --  ^ Ochotona princeps
              | Platypus --  ^ Ornithorhynchus anatinus 鸭嘴兽
              | Rabbit --  ^ Oryctolagus cuniculus
              | Medaka --  ^ Oryzias latipes 青鳉
              | Galago --  ^ Otolemur garnettii
              | Chimpanzee --  ^ Pan troglodytes 黑猩猩
              | Lamprey --  ^ Petromyzon marinus 七鳃鳗
              | Orangutan --  ^ Pongo abelii 红毛猩猩
              | Hyrax  --  ^ Procavia capensis 非洲蹄兔
              | Pteropus -- ^ Pteropus vampyrus 果蝠
              | TasmanianDevil --  ^ Sarcophilus harrisii
              | Shrew -- ^ Sorex araneus 鼩鼱
              | Squirrel -- ^ Spermophilus tridecemlineatus 松鼠
              | Boar -- ^ Sus scrofa
                deriving (Read,Eq)
                         
    
-- <option  value="tguttata">Taeniopygia guttata</option>
-- <option  value="trubripes">Takifugu rubripes</option>
-- <option  value="tsyrichta">Tarsius syrichta</option>
-- <option  value="tnigroviridis">Tetraodon nigroviridis</option>
-- <option  value="tbelangeri">Tupaia belangeri</option>
-- <option  value="ttruncatus">Tursiops truncatus</option>
-- <option  value="vpacos">Vicugna pacos</option>
-- <option  value="xtropicalis">Xenopus tropicalis</option>
-- <option disabled></option>
-- <option disabled>Ensembl Genomes Fungi</option>
-- <option disabled></option>
-- <option  value="aclavatus">Aspergillus clavatus</option>
-- <option  value="aflavus">Aspergillus flavus</option>
-- <option  value="afumigatus">Aspergillus fumigatus</option>
-- <option  value="anidulans">Aspergillus nidulans</option>
-- <option  value="aniger">Aspergillus niger</option>
-- <option  value="aoryzae">Aspergillus oryzae</option>
-- <option  value="aterreus">Aspergillus terreus</option>
-- <option  value="foxysporum">Fusarium oxysporum</option>
-- <option  value="gmoniliformis">Gibberella moniliformis</option>
-- <option  value="gzeae">Gibberella zeae</option>
-- <option  value="mgraminicola">Mycosphaerella graminicola</option>
-- <option  value="nhaematococca">Nectria haematococca</option>
-- <option  value="nfischeri">Neosartorya fischeri</option>
-- <option  value="ncrassa">Neurospora crassa</option>
-- <option  value="pnodorum">Phaeosphaeria nodorum</option>
-- <option  value="pgraministritici">Puccinia graministritici</option>
-- <option  value="ptriticina">Puccinia triticina</option>
-- <option  value="spombe">Schizosaccharomyces pombe</option>
-- <option  value="tmelanosporum">Tuber melanosporum</option>
-- <option  value="umaydis">Ustilago maydis</option>
-- <option disabled></option>
-- <option disabled>Ensembl Genomes Metazoa</option>
-- <option disabled></option>
-- <option  value="apisum">Acyrthosiphon pisum</option>
-- <option  value="aaegypti">Aedes aegypti</option>
-- <option  value="aqueenslandica">Amphimedon queenslandica</option>
-- <option  value="agambiae">Anopheles gambiae</option>
-- <option  value="amellifera">Apis mellifera</option>
-- <option  value="cquinquefasciatus">Culex quinquefasciatus</option>
-- <option  value="dpulex">Daphnia pulex</option>
-- <option  value="dananassae">Drosophila ananassae</option>
-- <option  value="derecta">Drosophila erecta</option>
-- <option  value="dgrimshawi">Drosophila grimshawi</option>
-- <option  value="dmojavensis">Drosophila mojavensis</option>
-- <option  value="dpersimilis">Drosophila persimilis</option>
-- <option  value="dpseudoobscura">Drosophila pseudoobscura</option>
-- <option  value="dsechellia">Drosophila sechellia</option>
-- <option  value="dsimulans">Drosophila simulans</option>
-- <option  value="dvirilis">Drosophila virilis</option>
-- <option  value="dwillistoni">Drosophila willistoni</option>
-- <option  value="dyakuba">Drosophila yakuba</option>
-- <option  value="iscapularis">Ixodes scapularis</option>
-- <option  value="nvectensis">Nematostella vectensis</option>
-- <option  value="phumanus">Pediculus humanus</option>
-- <option  value="smansoni">Schistosoma mansoni</option>
-- <option  value="spurpuratus">Strongylocentrotus purpuratus</option>
-- <option  value="tadhaerens">Trichoplax adhaerens</option>
-- <option disabled></option>
-- <option disabled>Ensembl Genomes Plants</option>
-- <option disabled></option>
-- <option  value="alyrata">Arabidopsis lyrata</option>
-- <option  value="athaliana">Arabidopsis thaliana</option>
-- <option  value="bdistachyon">Brachypodium distachyon</option>
-- <option  value="creinhardtii">Chlamydomonas reinhardtii</option>
-- <option  value="gmax">Glycine max</option>
-- <option  value="oglaberrima">Oryza glaberrima</option>
-- <option  value="osativa">Oryza sativa</option>
-- <option  value="oindica">Oryza sativa indica</option>
-- <option  value="ppatens">Physcomitrella patens</option>
-- <option  value="ptrichocarpa">Populus trichocarpa</option>
-- <option  value="smoellendorffii">Selaginella moellendorffii</option>
-- <option  value="sbicolor">Sorghum bicolor</option>
-- <option  value="vvinifera">Vitis vinifera</option>


