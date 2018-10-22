-- Modulo destinado a leitura e escrita de arquivos.

module InputOutput
(readTrainingSet,
 readTestSet,
 printEpoch
 ) where

import Types

type Image = [Float]
type Sample = (Int, Image)

-- Naturalmente, como todas as funcoes sao destinadas a leitura e
-- escrita de arquivos, todas (ou pelo menos a maioria) serao IO.

-- TODO
readTrainingSet :: [Sample]
readTrainingSet = []

-- TODO
readTestSet :: [Sample]
readTestSet = []

printEpoch :: Int -> Int -> Int -> IO()
printEpoch epochIndex correctCnt total = putStrLn $ "EPOCH #" ++ (show epochIndex) ++ " - " ++ (show correctCnt) ++ " / " ++ (show total)