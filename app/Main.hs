
module Main (main) where 

import REPL (repl)

main :: IO ()
main = do
  putStrLn "--- Welcome to the Simple Scheme interpreter! ---"
  repl []  -- Initially call with empty environment.
