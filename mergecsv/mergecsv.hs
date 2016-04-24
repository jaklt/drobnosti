import System.Directory
import System.Process
import System.Environment
import System.Exit
import Control.Monad
import Control.Applicative
import Data.List
import Control.Monad.Trans.Writer.Lazy

import Text.CSV

type StudentID = String
type Mark      = String
type ColumnNum = Int
type Row       = [String]
type MarkFun   = StudentID -> Maybe Mark


-- Here specify what to make the modification function of:
--   fnDom - index of domain column
--   fnTgt - index of target coulmn
-- (we're counting columns from 0)
fnDom, fnTgt :: ColumnNum
(fnDom, fnTgt) = (0,1)


-- Here specify how do we apply the above specified function to obtain the
-- resulting file::
--   toModifDom  - index of the column of input values
--   toModifTgt  - index of the column to modify
toModifDom, toModifTgt :: ColumnNum
(toModifDom, toModifTgt) = (2,5)

-- Files specification:
fnFileRowsToSkip, dataFileRowsToSkip :: Int
fnFileRowsToSkip = 1
dataFileRowsToSkip = 3

dataFile, dataFileRes :: String
dataFile    = "csvofstudents.csv"
dataFileRes = "new-csvofstudents.csv"

----------------------------------------------------------------


main :: IO ()
main = do
    args <- getArgs
    when (null args) $ do
        putStrLn "Usage: mergecsv <csv file>"
        exitWith ExitSuccess

    (markFun, studentsToMark) <- getMarking $ head args
    Right csvAll <- parseCSVFromFile dataFile
    let (skipped,csvRest) = splitAt dataFileRowsToSkip csvAll

    let (fixedCsvAll, markedStudents)
            = runWriter
            $ mapM (fixRow markFun toModifDom toModifTgt) csvRest

    writeFile dataFileRes $ printCSV (skipped ++ fixedCsvAll)

    putStrLn "Writing file finished."
    putStr   "Integrity check -- "

    let notMarked  = [ sID | sID <- studentsToMark, sID `notElem` markedStudents ]
        duplicates = studentsToMark \\ nub studentsToMark

    if length markedStudents == length studentsToMark
        then putStrLn "OK!"
        else do
            putStrLn "found inconsistencies:\n\n !!!\t!!!\n"
            forM_ notMarked print
            putStrLn $ "Duplicates: " ++ show duplicates ++ "\n"


fixRow :: MarkFun -> ColumnNum -> ColumnNum -> Row -> Writer [StudentID] Row
fixRow f dom tgt row
        | skipRow row = return row
        | otherwise   = go (row !! dom) tgt row
    where
        go index 0 (x:xs) = do
            case f index of
                Just result -> do
                    tell [index]
                    return $ result : xs

                Nothing -> return $ x:xs

        go index n (x:xs) = (x :) <$> go index (n-1) xs
        go _ _ [] = error $ "The following row is too short: " ++ show row

skipRow :: Row -> Bool
skipRow row =
    -- Keep the header rows unchanged.
    head row `elem` ["    Points Possible"]
    -- Ignore empty rows.
    || row == [""]


-- Example: ["","2179139","","","LI Functional Programming","31"]
-- produces f such that: f 2179139 = 31
--
-- (If there are multiple rows with the same ID, only the first counts.)
mkMarkFun :: [Row] -> MarkFun
mkMarkFun list = flip lookup list'
    where
        list' :: [(String, String)]
        list' = map fixPair $ filter ((== 2) . length) list

        fixPair row = (row !! fnDom, row !! fnTgt)

getMarking :: String -> IO (MarkFun, [StudentID])
getMarking fileName = do
    Right csvMarks <- parseCSVFromFile fileName

    let markedStudents
            = filter (/= "")
            $ map (!! fnDom)
            $ drop fnFileRowsToSkip csvMarks

    return $ (mkMarkFun csvMarks, markedStudents)

test :: IO ()
test = do
    (markFun, _) <- getMarking "classwork10.csv"
    Right csvAll <- parseCSVFromFile dataFile

    let checkedStudent = 3
        sRow = csvAll !! checkedStudent
        sID  = sRow !! toModifDom
        oldCol = sRow !! toModifTgt
        newCol = markFun sID
        (fixedRow, markedStudents)
                = runWriter
                $ fixRow markFun toModifDom toModifTgt sRow

    putStrLn $ "Before:"
    print $ sRow
    putStrLn $ "\nAfter:"
    print $ fixedRow
    putStrLn $ "\nStudentID: " ++ sID ++ ", oldCol: " ++ oldCol ++ ", newCol: " ++ show newCol
