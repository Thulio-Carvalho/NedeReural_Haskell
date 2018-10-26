-- Modulo destinado a leitura e escrita de arquivos.

module InputOutput
(getTraining,
 getTest,
 readIn,
 writeIn,
 isEmptyFile,
 definitiveAnswer,
 printEpoch
 ) where

import Data.List
import Data.Char
import Data.Maybe
import System.Directory
import System.Random
import Types
import Text.Printf
import Numeric.LinearAlgebra.HMatrix


type Image = [Double]
type Sample = (Int, Image)

strToArr::String->[Double]
strToArr line = [(read x::Double)| x<-(words line)]
    
strToMatrix::String->[[Double]]
strToMatrix mt = [(strToArr x)| x<-(lines mt)]
    
arrToStr::[Double]->String
arrToStr [] = ""
arrToStr (h:t) = (show h::String) ++ " " ++ (arrToStr t)
    
matrixToStr::[[Double]]->String
matrixToStr [] = ""
matrixToStr (h:t) = (arrToStr h) ++ "\n" ++ (matrixToStr t)

listRandom::Int->IO [Double]
listRandom 0 = return []
listRandom sz = do 
    head <- randomIO::IO Double
    tail <- listRandom (sz-1)
    return (head:tail)

matrixRandom::Int->Int->IO [[Double]]
matrixRandom 0 y = return []
matrixRandom x y = do
    head <- listRandom y
    tail <- matrixRandom (x-1) y
    return (head:tail)

getArchiveL::String->Int->IO [Double]
getArchiveL path sz = do
    emp <- isEmptyFile path
    if not emp then 
        do 
            archive <- (readFile path)
            return $ strToArr archive
    else 
        do
            ret <- listRandom sz
            return ret

getArchiveM::String->Int->Int->IO [[Double]]
getArchiveM path x y = do
    emp <- isEmptyFile path
    if not emp then 
        do 
            archive <- readFile path
            return $ strToMatrix archive
    else 
        do
            ret <- matrixRandom x y
            return ret

readIn::IO Data
readIn = do
    wH <- getArchiveM "Weight_hidden.txt" 30 784
    bH <- getArchiveL "Biases_hidden.txt" 30
    aH <- getArchiveL "Activation_hidden.txt" 30
    zH <- getArchiveL "Zeta_hidden.txt" 30

    wO <- getArchiveM "Weight_Output.txt" 10 30
    bO <- getArchiveL "Biases_Output.txt" 30
    aO <- getArchiveL "Activation_Output.txt" 30
    zO <- getArchiveL "Zeta_Output.txt" 30

    return $ (Data (fromLists wH) (fromList bH) (fromList aH) (fromList zH) (fromLists wO) (fromList bO) (fromList aO) (fromList zO))

writeIn::Data->IO() 
writeIn elem = do
    writeFile "Weight_hidden.txt" (matrixToStr (toLists $ wHidden elem))
    writeFile "Biases_hidden.txt" (arrToStr (toList $ bHidden elem))
    writeFile "Activation_hidden.txt" (arrToStr (toList $ aHidden elem))
    writeFile "Zeta_hidden.txt" (arrToStr (toList $ zetaHidden elem))

    writeFile "Weight_Output.txt" (matrixToStr (toLists $ wOutput elem))
    writeFile "Biases_Output.txt" (arrToStr (toList $ bOutput elem))
    writeFile "Activation_Output.txt" (arrToStr (toList $ aOutput elem))
    writeFile "Zeta_Output.txt" (arrToStr (toList $ zetaOutput elem))

isEmptyFile::String->IO Bool
isEmptyFile path = do
    elem <- readFile path

    if (length elem) == 0 then return True
    else return False

pickNumber::FilePath->Int
pickNumber "" = -1
pickNumber (h:t)
                | h == '-' = digitToInt $ t!!0
                | otherwise = (pickNumber t)

makeImage::FilePath->IO Image
makeImage path = do
    elem <- readFile path
    let ret = (strToArr elem)
    return ret

makeSample::FilePath->String->IO Sample
makeSample archive path = do
    let number = (pickNumber archive)
    mt <- (makeImage $ path++archive)
    return (number, mt)

listSample::[FilePath]->String->IO [Sample]
listSample [] path = return []
listSample (h:t) path = do
                head <- makeSample h path
                tail <- listSample t path
                return $ head:tail

getTest::IO [Sample]
getTest = do
    let caminho = "C:/Users/Amandio/Documents/Programacao/Hecome_Bumans/Tests" -- change
    elems <- (getDirectoryContents caminho) -- Colocar diretorio
    ret <- listSample (filter (/= ".") $ filter (/= "..") elems) caminho 
    return ret

getTraining::IO [Sample]
getTraining = do
    let caminho = "C:/Users/Amandio/Documents/Programacao/Hecome_Bumans/Trains" -- change
    elems <- (getDirectoryContents caminho) -- Colocar diretorio
    ret <- listSample (filter (/= ".") $ filter (/= "..") elems) caminho 
    return ret

printEpoch :: Int -> Int -> Int -> IO()
printEpoch epochIndex correctCnt total = putStrLn $ "EPOCH #" ++ (show epochIndex) ++ " - " ++ (show correctCnt) ++ " / " ++ (show total)

-- Recebe a lista de ativacao como parametro
-- Retorna uma tupla: o melhor sigmoid e seu respectivo indice
getBestSigmoid :: [Double] -> (Double, Int)
getBestSigmoid activationValues = let first = maximum (activationValues)
                                      second = fromJust $ (elemIndex (maximum (activationValues)) (activationValues))
                                  in (first, second)
                                  
-- Recebe a tupla (sigmoid, indice) 
-- Retorna a formatacao em string que representa
-- o numero e a probabilidade de ser esse numero
toPercentage :: (Double, Int) -> [Double] -> String
toPercentage sigmoid activationValues = 
    "[" ++ (printf "%d" (snd sigmoid + 1)) ++ " - " ++ (printf "%.2f" ((fst sigmoid) * 100 / sum (activationValues))) ++ "%]" 

-- Computa a resposta definitiva a partir do
-- array de valores de ativacao
definitiveAnswer :: Data -> IO String
definitiveAnswer activationValues = return ("Resposta definitiva: " ++ toPercentage (getBestSigmoid (toList $ aOutput activationValues)) (toList $ aOutput activationValues))