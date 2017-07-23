module Main where
import System.Exit
import TLE.Base
import TLE.Net
import TLE.Database

test :: IO [Site]
test = do
    content <- readFile "testdata.sat"
    return $ [ Site (Src "" "") (lines content) ]

main = do
    sites <- downloadData
    --sites <- test
    if (length sites) == 0
    then putStrLn "[-] No sites specified in Net.hs... add some!" >> exitFailure
    else return ()
    let tles = concat $ map (buildTLEs . content) sites
    putStrLn $ "[+] Constructed " ++ (show . length) tles ++ " TLE entries."
    putStrLn $ "[+] From " ++ show ((length tles) * 3) ++ " source lines."
    saveToDB tles
