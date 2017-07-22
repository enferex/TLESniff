module TLE.Base where

import Data.Char

testData = "GPS BIIR-07             \n1 26690U 01004A   17184.29217997  .00000043  00000-0  00000-0 0  9992\n2 26690  53.0436 157.1577 0183729 257.2811 269.4047  2.00569882120347\nGPS BIIR-07             \n1 26690U 01004A   17184.29217997  .00000043  00000-0  00000-0 0  9992\n2 26690  53.0436 157.1577 0183729 257.2811 269.4047  2.00569882120347\n"

data TLEValue = I Int | S String | D Double | C Char
data TLEField = TLEField {name::String, value::TLEValue}
type TLE = [TLEField]

instance Show TLEValue where
    show (I x) = show x
    show (D x) = show x
    show (C x) = "\"" ++ (show x) ++ "\""
    show (S x) = "\"" ++ x ++ "\""

trim :: String -> String
trim xs = filter (\x -> not $ isSpace x) xs

subStr :: Int -> Int -> String -> String
subStr lo hi xs = take (1 + hi - lo) $ drop lo xs

valFind a b c = read $ subStr a b c
intFrom a b c = valFind a b c :: Int
fltFrom a b c
    | (c !! a) == ' ' = fltFrom (a+1) b c
    | (c !! a) == '-' = -1.0 * fltFrom (a+1) b c
    | (c !! a) == '.' = fltFrom 0 (b-a+2) $ ('0' : (subStr a b c))
    | otherwise = valFind a b c :: Double

fltAss a b c 
    | (c !! a) == '-' =
        read $ "-0." ++ (subStr (a+1) (b-2) c) ++ ['e'] ++ [(c !! b)] :: Double
    | otherwise =
        read $ "0." ++ (subStr (a+1) (b-2) c) ++ ['e'] ++ [(c !! b)] :: Double

buildTLE :: String -> String -> String -> TLE
buildTLE name l1 l2 = [ -- TLE Line 0
                        TLEField "name"           $ S (trim name)
                        -- TLE Line 1
                      , TLEField "satNo"          $ I (intFrom 2 6 l1)
                      , TLEField "classification" $ C (l1 !! 7)
                      , TLEField "launchYear"     $ I (intFrom 9  10 l1)
                      , TLEField "launchNumber"   $ I (intFrom 11 13 l1)
                      , TLEField "launchPiece"    $ S (trim $ take 3 $ drop 14 l1)
                      , TLEField "epochYear"      $ I (intFrom 18 19 l1)
                      , TLEField "epochDay"       $ D (fltFrom 20 31 l1)
                      , TLEField "fstMeanMotion"  $ D (fltFrom 33 42 l1)
                      , TLEField "sndMeanMotion"  $ D (fltAss 44 51  l1)
                      , TLEField "bstarDrag"      $ D (fltAss 53 60  l1)
                      , TLEField "ephermisType"   $ I (intFrom 62 62 l1)
                      , TLEField "elementNo"      $ I (intFrom 64 67 l1)
                      , TLEField "checksumLine1"  $ I (intFrom 68 68 l1)
                      -- TLE Line 2
                      , TLEField "catNumber"      $ I (intFrom 2 6   l2)
                      , TLEField "orbInclination" $ D (fltFrom 8 15  l2)
                      , TLEField "rightAscension" $ D (fltFrom 17 24 l2)
                      , TLEField "eccentricity"   $ D (fltFrom 26 32 l2)
                      , TLEField "argPerigee"     $ D (fltFrom 34 41 l2)
                      , TLEField "meanAnomaly"    $ D (fltFrom 43 50 l2)
                      , TLEField "meanMotion"     $ D (fltFrom 52 62 l2)
                      , TLEField "revNumEpoch"    $ I (intFrom 63 67 l2)
                      , TLEField "checksumLine2"  $ I (intFrom 668 68 l2) ]

buildTLEs' :: [String] -> [TLE] -> [TLE]
buildTLEs' (n:l1:l2:xs) ts = buildTLEs' xs ((buildTLE n l1 l2) : ts)
buildTLEs' [] ts = ts
buildTLEs' (n:line) ts = ts

buildTLEs :: [String] -> [TLE]
buildTLEs xs = buildTLEs' xs []
