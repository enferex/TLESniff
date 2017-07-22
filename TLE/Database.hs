module TLE.Database where
import TLE.Base
import Database.SQLite

databaseName = "test.sql"

chop    xs = take ((length xs)-1) xs
join    xs = (chop . concat) $ [x ++ " " ++ y ++ "," | (x,y) <- xs]
join'   xs = (chop . concat) $ [x ++ "," | x <- xs]
fields tle = map name tle
values tle = map value tle
types  tle = map typeOf tle

typeOf :: TLEField -> String
typeOf TLEField {name=_, value=I{}} = "INTEGER"
typeOf TLEField {name=_, value=S{}} = "TEXT"
typeOf TLEField {name=_, value=C{}} = "TEXT"
typeOf TLEField {name=_, value=D{}} = "REAL"

initDB :: TLE -> IO SQLiteHandle
initDB tle = do
    putStrLn $ "[+] Initializing database " ++ databaseName
    db <- openConnection databaseName
    execStatement_ db q
    return db
    where
        q = " CREATE TABLE IF NOT EXISTS tbl (" ++ t ++ ")"
        t = join $ zip (fields tle) (types tle)

updateEntry :: SQLiteHandle -> TLE -> IO SQLiteHandle
updateEntry db tle = execStatement_ db q >> return db
    where
        q = "INSERT INTO tbl ( " ++ f ++ ") VALUES (" ++ v ++ ")"
        f = join' $ fields tle
        v = join' $ map (show) (values tle)

updateDB :: [TLE] -> SQLiteHandle -> IO SQLiteHandle
updateDB tles db = mapM (updateEntry db) tles >> return db

saveToDB :: [TLE] -> IO ()
saveToDB tles = initDB (head tles) >>= updateDB tles >>= closeConnection