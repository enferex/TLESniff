module Main where
import Control.Concurrent
import System.Exit
import System.Environment
import TLE.Base
import TLE.Net
import TLE.Database

delaySec n = n * 1000000 -- Microseconds to seconds

test :: IO [Site]
test = do
    content <- readFile "testdata.tle"
    return $ [ Site (Src "" "") (lines content) ]

usage :: IO ()
usage = do
    pgm <- getProgName
    die $ "Usage: " ++ pgm ++ " [URL to tle file...]" 

run :: [Source] -> Int -> IO ()
run sources n = do
    sites <- downloadData sources
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
        min = 8 * 60
        ms  = (1000000 * 60 * min) -- Microseconds to minutes

sourceFromCLI :: [String] -> [Source]
sourceFromCLI args = map (\x -> Src x "CLI Supplied Data Source") args

toURL  s = fst $ span (/= ' ') s
toDesc s = snd $ span (/= ' ') s

sourceFromFile :: String -> IO [Source]
sourceFromFile fname = do
    infoz <- readFile fname
    return $ map (\x -> Src (toURL x) (toDesc x)) (lines infoz)

main :: IO ()
main = do
    args <- getArgs
    if (length args) == 0 then usage else return ()
    srcs <- sourceFromFile $ head args
    mapM_ (\x -> run srcs x >> delay >> return ()) [1..]

main' :: IO ()
main' = do
    args <- getArgs
    if (length args) == 0 then usage else return ()
    let srcs = sourceFromCLI $ args
    mapM_ (\x -> run srcs x >> delay >> return ()) [1..]
