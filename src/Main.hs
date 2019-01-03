{-# LANGUAGE TupleSections #-}

module Main(main) where

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.Time
import Data.Time.Calendar.WeekDate
import Data.Tuple
import Data.List
import Data.List.Extra
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import qualified System.IO.Strict as S

longPause :: Int
longPause = 30 -- minutes

period :: Int
period = 1 -- minutes

database :: FilePath
database = ".tt"

type Slot = (LocalTime, LocalTime)

type DayLog = [Slot]

getTime :: IO LocalTime
getTime = do
    utcTime <- getCurrentTime
    tz <- getTimeZone utcTime
    return $ utcToLocalTime tz utcTime

updateLog :: DayLog -> LocalTime -> DayLog
updateLog [] t = [(t, t)]
updateLog logs@((t0, t1) : slots) t
    | diffTime t t1 > longPause*60 = (t, t) : logs
    | otherwise = (t0, t) : slots

diffTime :: LocalTime -> LocalTime -> Int
diffTime t0 t1 =
    let dt = timeOfDayToTime (localTimeOfDay t0) - timeOfDayToTime (localTimeOfDay t1)
        pico = diffTimeToPicoseconds dt
    in fromIntegral $ pico `div` 1000000000000

getLogFileName :: IO FilePath
getLogFileName = do
    home <- getHomeDirectory
    timestamp <- showGregorian . localDay <$> getTime
    let dir = home </> database
    isDir <- doesPathExist dir
    unless isDir $ createDirectory dir
    return $ home </> database </> timestamp

readLogs :: FilePath -> IO DayLog
readLogs logFileName = reverse . read <$> S.readFile logFileName

writeLogs :: FilePath -> DayLog -> IO ()
writeLogs logFileName = writeFile logFileName . pretty . reverse

pretty :: DayLog -> String
pretty logs = "[ " ++ intercalate "\n, " (map show logs) ++ "\n]"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [ "-d" ] -> daemon
        [] -> report
        _ -> usage

usage :: IO ()
usage = do
    putStrLn "tt: Time Tracker"
    putStrLn "Usage:"
    putStrLn "    tt -d: start the tt daemon"
    putStrLn "    tt   : print the current timesheet"
    exitFailure

daemon :: IO ()
daemon = do
    logFileName <- getLogFileName
    isFile <- doesFileExist logFileName
    logs <- if isFile
        then readLogs logFileName
        else return []
    loop logFileName logs

loop :: FilePath -> DayLog -> IO ()
loop fileName logs = do
    t <- getTime
    if newDay logs t
        then daemon
        else do
            let logs' = updateLog logs t
            writeLogs fileName logs'
            wait >> loop fileName logs'

newDay :: DayLog -> LocalTime -> Bool
newDay [] _ = False
newDay ((_, t1):_) t = localDay t /= localDay t1

wait :: IO ()
wait = threadDelay $ period * 60 * 1000000

report :: IO ()
report = do
    home <- getHomeDirectory
    days <- sort <$> listDirectory (home </> database)
    logs <- groupOn (weekNumber . snd) <$> do
        forM days $ \day ->
            (day, ) <$> readLogs (home </> database </> day)
    let plain = unlines $ intercalate [""] $ map (mapMaybe showLog) logs
    putStr plain

weekNumber :: DayLog -> Maybe Int
weekNumber [] = Nothing
weekNumber ((t,_):_) = Just wn
    where (_year, wn, _dow) = toWeekDate (localDay t)

showLog :: (String, DayLog) -> Maybe String
showLog (_, []) = Nothing
showLog (day, logs) =
    let s = sum $ map (uncurry diffTime . swap) logs
        (h, m) = (s `div` 60) `divMod` 60
        h' = (if h < 10 then " " else "") ++ show h
        m' = (if m < 10 then "0" else "") ++ show m
    in Just $ day ++ ": " ++ h' ++ ":" ++ m'
