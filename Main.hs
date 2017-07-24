module Main where
import Control.Concurrent
import System.Exit
import TLE.Base
import TLE.Net
import TLE.Database

delaySec n = n * 1000000 -- Microseconds to seconds

test :: IO [Site]
test = do
    content <- readFile "testdata.sat"
    return $ [ Site (Src "" "") (lines content) ]

run :: Int -> [Site] -> IO ()
run n sites = do
    let tles = concat $ map (buildTLEs . content) sites
    putStrLn $ "[+] --[ Sample Session " ++ show n ++ " ]--"
    putStrLn $ "[+] Constructed " ++ (show . length) tles ++ " TLE entries."
    putStrLn $ "[+] From " ++ show ((length tles) * 3) ++ " source lines."
    saveToDB tles

delay :: IO ()
delay = do
    putStrLn $ "[+] Delaying for " ++ show min ++ " minute(s)."
    threadDelay ms >> putStrLn ""
    where
        min = 1
        ms  = (1000000 * 60 * min) -- Microseconds to minutes

main = do
    --sites <- downloadData
    sites <- test
    if (length sites) == 0
    then putStrLn "[-] No sites specified in Net.hs... add some!" >> exitFailure
    else return ()
    mapM_ (\x ->  run x sites >> delay >> return ()) [1..]
