module TLE.Net where

import Network.Curl

data Source = Src  {url::URLString, desc::String} deriving(Show)
data Site   = Site {src::Source, content::[String]} deriving(Show)

getWebContent :: Source -> IO Site
getWebContent src = do
    putStrLn $ "[+] Downloading content from: " ++ url src ++ " (" ++ desc src ++ ")"
    (err, content) <- curlGetString (url src) []
    case err of
        CurlOK -> do
            let xs = (lines content)
            putStrLn $ "[+] Found " ++ (show.length) xs ++ " lines of TLE data."
            return $ Site src xs
        _  -> return $ Site src []

downloadData :: [Source] -> IO [Site]
downloadData sources = mapM getWebContent sources
