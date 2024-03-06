import System.Random (randomRIO)

main :: IO ()
main = do
    putStrLn "Enter the size of your die:"
    dieSizeStr <- getLine
    let dieSize = read dieSizeStr :: Int

    putStrLn $"Press Enter to roll the " ++ show dieSize ++ "-sided die."
    _ <- getLine
    roll <- rollDie dieSize
    putStrLn $"You rolled: " ++ show roll ++ "!"

rollDie :: Int -> IO Int
rollDie dieSize = randomRIO (1, dieSize)