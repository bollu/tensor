module Main where
import Tensor
import Instances

ex1 :: M Int
ex1 = do
 return $ 1

runex :: Show a => String -> M a -> IO ()
runex name m = do
  let a = runcompiler $ compileM m
  putStrLn $ "##" <> name <> "##"
  print a

main :: IO ()
main = do
   runex "ex1" ex1

