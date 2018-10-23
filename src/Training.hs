-- Modulo destinado a todas as funcoes relativas ao treino da Rede.

module Training 
(train) where

import Execution
import InputOutput
import Types
import Data.List.Split

type Image = [Float]
type Sample = (Int, Image)

train :: Int -> IO String
train epochAmount = manageEpoch epochAmount trainingSet testSet network
                -- save network
                where   trainingSet = readTrainingSet
                        network = initialize
                        testSet = readTestSet

manageEpoch :: Int -> [Sample] -> [Sample] -> Data -> IO String
manageEpoch 0 _ _ _ = return ""
manageEpoch epochAmount trainingSets testSet network = do
                                                let newNetwork = trainingEpoch trainingSets network
                                                    correctCnt = testEpoch testSet newNetwork
                                                    totalAmount = length testSet
                                                do  return $ printEpoch epochAmount correctCnt totalAmount
                                                    manageEpoch (epochAmount - 1) trainingSets testSet newNetwork

trainingEpoch :: [Sample] -> Data -> Data
trainingEpoch trainingSet network = let minibatchAmount = 20 -- adequar quantidade
                                        minibatchSize = (length trainingSet) `div` 20 -- adequar quantidade
                                        -- dar shuffle
                                        minibatches = chunksOf minibatchSize trainingSet
                                        newNetwork = manageMinibatch minibatchAmount 0 network minibatches
                                    in newNetwork

manageMinibatch :: Int -> Int -> Data -> [[Sample]] -> Data
manageMinibatch amount counter network minibatches = if amount /= (counter + 1) 
                                                        then  
                                                          let changes = minibatchEvaluation (minibatches !! counter) amount
                                                          in plus (plus network changes) (manageMinibatch amount (counter + 1) network minibatches)
                                                        else
                                                          generateBasedOf network 

-- TODO
minibatchEvaluation :: [Sample] -> Int -> Data
minibatchEvaluation minibatch amount = Data [[]] [] [[]] []

testEpoch :: [Sample] -> Data -> Int
testEpoch _ _ = 5

-- Recebe as informacoes da rede neural, o resultado 
-- esperado e retorna um Data com as modificacoes necessarias
-- na rede
backpropagation :: Data -> Int -> Data
backpropagation network expected
     | isEmpty network = error "Data is empty"
     | expected < 0 || expected > 9 = error "Invalid expected number"
     | otherwise = generateBasedOf network -- MUDAR, SÓ PRA RODAR
                 -- TODO