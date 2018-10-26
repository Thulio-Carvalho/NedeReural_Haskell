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


type Image = [Float]
type Sample = (Int, Image)

-- transforma uma string em uma lista
strToArr::String->[Float]
strToArr line = [(read x::Float)| x<-(words line)]

-- transforma uma strig em uma matriz 
strToMatrix::String->[[Float]]
strToMatrix mt = [(strToArr x)| x<-(lines mt)]

-- transforma uma lista em uma string
arrToStr::[Float]->String
arrToStr [] = ""
arrToStr (h:t) = (show h::String) ++ " " ++ (arrToStr t)

-- transforma uma matriz em uma string
matrixToStr::[[Float]]->String
matrixToStr [] = ""
matrixToStr (h:t) = (arrToStr h) ++ "\n" ++ (matrixToStr t)

-- dado o seu tamanho
-- gera uma lista com valores randomicos
listRandom::Int->IO [Float]
listRandom 0 = return []
listRandom sz = do 
    head <- randomIO::IO Float
    tail <- listRandom (sz-1)
    return (head:tail)

-- dado o seu tamanho
-- gera uma matriz com valores randomicos
matrixRandom::Int->Int->IO [[Float]]
matrixRandom 0 y = return []
matrixRandom x y = do
    head <- listRandom y
    tail <- matrixRandom (x-1) y
    return (head:tail)

-- dado seu caminho
-- le o arquivo e o transforma em uma lista
getArchiveL::String->Int->IO [Float]
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

-- dado seu caminho
-- le o arquivo e o transforma em uma matriz
getArchiveM::String->Int->Int->IO [[Float]]
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

-- le os datas presente nos arquivos
readIn::IO Data
readIn = do
    wH <- getArchiveM "NedeReural_Haskell/Data/Weight_hidden.txt" 30 784
    bH <- getArchiveL "NedeReural_Haskell/Data/Biases_hidden.txt" 30
    aH <- getArchiveL "NedeReural_Haskell/Data/Activation_hidden.txt" 30
    zH <- getArchiveL "NedeReural_Haskell/Data/Zeta_hidden.txt" 30

    wO <- getArchiveM "NedeReural_Haskell/Data/Weight_Output.txt" 10 30
    bO <- getArchiveL "NedeReural_Haskell/Data/Biases_Output.txt" 30
    aO <- getArchiveL "NedeReural_Haskell/Data/Activation_Output.txt" 30
    zO <- getArchiveL "NedeReural_Haskell/Data/Zeta_Output.txt" 30

    return $ Data wH bH aH zH wO bO aO zO

-- escreve o data nos arquivos
writeIn::Data->IO() 
writeIn elem = do
    writeFile "NedeReural_Haskell/Data/Weight_hidden.txt" (matrixToStr (wHidden elem))
    writeFile "NedeReural_Haskell/Data/Biases_hidden.txt" (arrToStr (bHidden elem))
    writeFile "NedeReural_Haskell/Data/Activation_hidden.txt" (arrToStr (aHidden elem))
    writeFile "NedeReural_Haskell/Data/Zeta_hidden.txt" (arrToStr (zetaHidden elem))

    writeFile "NedeReural_Haskell/Data/Weight_Output.txt" (matrixToStr (wOutput elem))
    writeFile "NedeReural_Haskell/Data/Biases_Output.txt" (arrToStr (bOutput elem))
    writeFile "NedeReural_Haskell/Data/Activation_Output.txt" (arrToStr (aOutput elem))
    writeFile "NedeReural_Haskell/Data/Zeta_Output.txt" (arrToStr (zetaOutput elem))

-- verifica se o arquivo esta vazio
isEmptyFile::String->IO Bool
isEmptyFile path = do
    elem <- readFile path

    if (length elem) == 0 then return True
    else return False

-- pega o numero que define a resposta do teste/treino
pickNumber::FilePath->Int
pickNumber "" = -1
pickNumber (h:t)
                | h == '-' = digitToInt $ t!!0
                | otherwise = (pickNumber t)

-- Constroi uma imagem
makeImage::FilePath->IO Image
makeImage path = do
    elem <- readFile path
    let ret = (strToArr elem)
    return ret

-- constroi um Sample
makeSample::FilePath->String->IO Sample
makeSample archive path = do
    let number = (pickNumber archive)
    mt <- (makeImage $ path++archive)
    return (number, mt)

-- cria uma lista de Sample dado os arquivos e o local
listSample::[FilePath]->String->IO [Sample]
listSample [] path = return []
listSample (h:t) path = do
                head <- makeSample h path
                tail <- listSample t path
                return $ head:tail

-- retorna uma lista com os testes
getTest::IO [Sample]
getTest = do
    let caminho = "NedeReural_Haskell/Tests" -- Diretorio de testes
    elems <- (getDirectoryContents caminho) -- Colocar diretorio
    ret <- listSample (filter (/= ".") $ filter (/= "..") elems) caminho 
    return ret

-- retorna uma lista com os treinos
getTraining::IO [Sample]
getTraining = do
    let caminho = "NedeReural_Haskell/Training" -- Diretorio de treino
    elems <- (getDirectoryContents caminho) -- Colocar diretorio
    ret <- listSample (filter (/= ".") $ filter (/= "..") elems) caminho 
    return ret

printEpoch :: Int -> Int -> Int -> IO()
printEpoch epochIndex correctCnt total = putStrLn $ "EPOCH #" ++ (show epochIndex) ++ " - " ++ (show correctCnt) ++ " / " ++ (show total)

-- Recebe a lista de ativacao como parametro
-- Retorna uma tupla: o melhor sigmoid e seu respectivo indice
getBestSigmoid :: [Float] -> (Float, Int)
getBestSigmoid activationValues = let first = maximum (activationValues)
                                      second = fromJust $ (elemIndex (maximum (activationValues)) (activationValues))
                                  in (first, second)
                                  
-- Recebe a tupla (sigmoid, indice) 
-- Retorna a formatacao em string que representa
-- o numero e a probabilidade de ser esse numero
toPercentage :: (Float, Int) -> [Float] -> String
toPercentage sigmoid activationValues = 
    "[" ++ (printf "%d" (snd sigmoid + 1)) ++ " - " ++ (printf "%.2f" ((fst sigmoid) * 100 / sum (activationValues))) ++ "%]" 

-- Computa a resposta definitiva a partir do
-- array de valores de ativacao
definitiveAnswer :: Data -> IO String
definitiveAnswer activationValues = return ("Resposta definitiva: " ++ toPercentage (getBestSigmoid $ aOutput activationValues) (aOutput activationValues))