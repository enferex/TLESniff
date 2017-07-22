module Main where
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
    let tles = concat $ map (buildTLEs . content) sites
    putStrLn $ "[+] Constructed " ++ (show . length) tles ++ " TLE entries."
    putStrLn $ "[+] From " ++ show ((length tles) * 3) ++ " source lines."
    saveToDB tles
