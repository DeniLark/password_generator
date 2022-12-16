import           Control.Monad
import           Control.Monad.State
import           System.Environment
import           System.Random

main :: IO ()
main = do
  (passLength : upChar : numChar : _) <- (<> repeat "y") <$> getArgs
  gen <- getStdGen
  let password = evalState
        (randomString (collectString upChar numChar) $ read passLength)
        gen
  putStrLn password

collectString :: String -> String -> [String]
collectString upChar numChar = filter
  (not . null)
  [ ['a' .. 'z']
  , if upChar == "y" || upChar == "Y" then ['A' .. 'Z'] else []
  , if numChar == "y" || numChar == "Y" then ['0' .. '9'] else []
  ]

randomRSt :: Random a => (a, a) -> State StdGen a
randomRSt = state . randomR

randomChar :: [String] -> State StdGen Char
randomChar chars = do
  cs <- (chars !!) <$> randomRSt (0, length chars - 1)
  (cs !!) <$> randomRSt (0, length cs - 1)


randomString :: [String] -> Int -> State StdGen String
randomString ss = (`replicateM` randomChar ss)


-- randomChar' :: State StdGen Char
-- randomChar' = (chars !!) <$> randomRSt (0, length chars - 1)
--  where
--   chars :: String
--   chars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']

-- randomString' :: Int -> State StdGen String
-- randomString' = (`replicateM` randomChar')

