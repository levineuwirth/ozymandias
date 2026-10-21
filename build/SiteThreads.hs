-- | Read the compile worker setting without starting the runtime workers.
module SiteThreads (siteThreads) where

import System.Environment (lookupEnv)
import System.IO          (hPutStrLn, stderr)
import Text.Read          (readMaybe)

-- | How many items Hakyll compiles at once: @SITE_THREADS@, a whole number
-- from 1 to 'maxThreads', or 'defaultThreads'. Hakyll runs one worker per
-- capability, and the generator used to get one. Measured 2026-10-06
-- (8 cores, 16 threads, under load): a full compile took 33 s on 1, 15 s
-- on 4, 14 s on 8 and 18 s on 16, where the parallel collector's overhead
-- outgrows the work; a no-change build, mostly Hakyll's single-threaded
-- dependency check, was the same on 1 and 4 and a second slower on 8.
-- Output is byte-identical whatever the count.
defaultThreads :: Int
defaultThreads = 4

-- | The threaded RTS's MAX_N_CAPABILITIES (rts/Config.h).
maxThreads :: Integer
maxThreads = 256

-- | Read as an 'Integer' and range-checked before it becomes an 'Int':
--   read straight into 'Int', an out-of-range value wrapped round and
--   passed (-18446744073709551614 gave 2; 4294967296 reached the RTS,
--   which warned and used 1).
siteThreads :: IO Int
siteThreads = do
    setting <- lookupEnv "SITE_THREADS"
    case setting of
        Nothing -> return defaultThreads
        Just "" -> return defaultThreads
        Just s  -> case readMaybe s :: Maybe Integer of
            Just n | n >= 1, n <= maxThreads -> return (fromInteger n)
            _ -> do
                hPutStrLn stderr ("site: SITE_THREADS=" ++ show s ++ " is not a whole number from 1 to "
                                  ++ show maxThreads ++ "; using " ++ show defaultThreads)
                return defaultThreads
