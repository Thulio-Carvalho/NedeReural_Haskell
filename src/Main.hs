-- Modulo principal de Nede Reural.

module Main where
import Training
import Execution
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    menu (args)
    return ()-- action apenas para satisfazer o retoro de IO ().

menu :: [String] -> IO String -- a decidir o tipo da IO Action
menu (x:xs)
    | x == "exec" = do 
                    answer <- execute
                    putStrLn answer
                    return ""
    | x == "train" = train (read (head xs))
    | otherwise = return ("Opcao invalida!")