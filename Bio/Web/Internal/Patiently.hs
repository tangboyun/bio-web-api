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
module Bio.Web.Internal.Patiently 
       (
         patiently
       , sleep
       )
       where
import Control.Concurrent
import Network.Shpider
import System.IO


patiently :: Int -> (a ->  Shpider (ShpiderCode, Page)) -> a 
          -> Shpider (ShpiderCode, Page)
patiently waitTime f v = do
  res@(c,_) <- f v
  case c of
    Ok -> return res
    _  -> do
      let waitTime' = if waitTime <= 3600
                      then waitTime * 2
                      else 3600
      liftIO $ hPutStrLn stderr 
              ("ERROR: " ++ show c ++ 
               "\nWaiting " ++ show waitTime ++ 
               " seconds...") 
      liftIO $ sleep waitTime
      patiently waitTime' f v
  where
    
sleep :: Int -> IO ()    
sleep = threadDelay . (* 1000000)
