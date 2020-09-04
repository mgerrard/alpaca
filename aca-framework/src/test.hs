import Control.Monad (when)

guarded :: IO ()
guarded = when True $ do
  putStrLn "Hello"
  putStrLn "there"

