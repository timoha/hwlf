
import Entry
import System.Environment
import System.Directory


save :: Entry -> IO ()
save entry = appendFile "myjornal.txt" ((show entry) ++ "\n")

main = do
    args <- getArgs
    jornalEntry <- parseArgs args
    case jornalEntry of
        Nothing    -> putStrLn "Invalid format"
        Just entry -> do save entry
                         putStrLn $ show entry