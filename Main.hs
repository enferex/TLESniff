module Main where
import Control.Concurrent
import System.Console.ArgParser
import System.Exit
import System.Environment
import TLE.Base
import TLE.Net
import TLE.Database

delaySec n = n * 1000000 -- Microseconds to seconds

data Opt = Opt String Int deriving(Show)

argParser :: ParserSpec Opt 
argParser = Opt `parsedBy` optFlag "" "url file" `andBy` optFlag 0 "delay minutes"

test :: IO [Site]
test = do
    content <- readFile "testdata.tle"
    return $ [ Site (Src "" "") (lines content) ]

run :: [Source] -> Int -> IO ()
run sources n = do
    sites <- downloadData sources
    let tles = concat $ map (buildTLEs . content) sites
    putStrLn $ "[+] --[ Sample Session " ++ show n ++ " ]--"
    putStrLn $ "[+] Constructed " ++ (show . length) tles ++ " TLE entries."
    putStrLn $ "[+] From " ++ show ((length tles) * 3) ++ " source lines."
    saveToDB tles

delay :: Int -> IO ()
delay d = do
    putStrLn $ "[+] Delaying for " ++ show d ++ " minute(s)."
    threadDelay ms >> putStrLn ""
    where
        ms  = (1000000 * 60 * d) -- Microseconds to minutes

sourceFromCLI :: [String] -> [Source]
sourceFromCLI args = map (\x -> Src x "CLI Supplied Data Source") args

toURL  s = fst $ span (/= ' ') s
toDesc s = snd $ span (/= ' ') s

sourceFromFile :: String -> IO [Source]
sourceFromFile fname = do
    infoz <- readFile fname
    return $ map (\x -> Src (toURL x) (toDesc x)) (lines infoz)

main' :: Opt -> IO ()
main'(Opt u d) = do
    if (length u) == 0 then exitSuccess else return ()
    srcs <- sourceFromFile u
    mapM_ (\x -> run srcs x >> delay d >> return ()) z
    where
        z | d > 0 = [1..]
          | otherwise = [1]

main :: IO ()
main = do
    parser <- mkApp argParser
    runApp parser main'
