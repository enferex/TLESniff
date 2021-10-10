module TLE.Database where
import TLE.Base
import TLE.Net
import Database.SQLite3
import qualified Data.Text as Txt

databaseName = "TLEs.sql"

chop     xs = take ((length xs)-1) xs
join     xs = (chop . concat) $ [x ++ " " ++ y ++ "," | (x,y) <- xs]
join'    xs = (chop . concat) $ [x ++ "," | x <- xs]
fields' tle = map name (fields tle)
values  tle = map value (fields tle)
types   tle = map typeOf (fields tle)

typeOf :: TLEField -> String
typeOf TLEField {name=_, value=I{}} = "INTEGER"
typeOf TLEField {name=_, value=S{}} = "TEXT"
typeOf TLEField {name=_, value=C{}} = "TEXT"
typeOf TLEField {name=_, value=D{}} = "REAL"

initDB :: TLE -> IO Database
initDB tle = do
    putStrLn $ "[+] Initializing database " ++ databaseName
    db <- open $ Txt.pack databaseName
    exec db (Txt.pack q) >> return db
    where
        q = "CREATE TABLE IF NOT EXISTS tles \
            \(timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,\
            \ source TEXT," ++ t ++");\
            \ CREATE UNIQUE INDEX IF NOT EXISTS id ON tles \
            \ (satNo, elementNo, checksumLine1, checksumLine2);"
        t = join $ zip (fields' tle) (types tle)

updateEntry :: Database -> TLE -> IO Database
updateEntry db tle = exec db (Txt.pack q) >> return db
    where
        q = "INSERT OR IGNORE INTO tles ( source, " ++ f ++ ")\
            \VALUES (" ++ s ++ ", " ++ v ++ ");"
        f = join' $ fields' tle
        v = join' $ map (show) (values tle)
        s = (show . url . source) tle

updateDB :: [TLE] -> Database -> IO Database
updateDB tles db = mapM (updateEntry db) tles >> return db

saveToDB :: [TLE] -> IO ()
saveToDB tles = initDB (head tles) >>= updateDB tles >>= close
