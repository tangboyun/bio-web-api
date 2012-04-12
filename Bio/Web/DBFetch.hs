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
module Bio.Web.DBFetch
       
       
       where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack,unpack)
import Network.Shpider
import Bio.Web.DBFetch.Types


seqFetch :: [ByteString] -> IO SeqRecord
seqFetch geneIds = runShpider $ do
