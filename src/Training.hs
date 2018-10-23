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
train epochAmount = manageEpoch epochAmount trainingSets testSet network
                    where   trainingSets = readTrainingSet
                            network = initialize
                            testSet = readTestSet

manageEpoch :: Int -> [Sample] -> [Sample] -> Data -> IO String
manageEpoch 0 _ _ _ = return ""
manageEpoch epochAmount trainingSets testSet network = do
                                                let correctCnt = testEpoch testSet network
                                                    trainingEpoch trainingSets network
                                                    printEpoch epochAmount correctCnt (length testSet)
                                                    manageEpoch (epochAmount - 1) trainingSets testSet network

trainingEpoch :: [Sample] -> Data -> IO String
trainingEpoch trainingSet network = let minibatchAmount = 20 -- adequar quantidade
                                        minibatchSize = 500 -- adequar quantidade
                                        -- dar shuffle
                                        minibatches = chunksOf minibatchSize trainingSet -- ajustar dependencias
                                        newNetwork = manageMinibatch minibatchAmount 0 network minibatches
                                        -- chamar funcao de save com a nova rede
                                        return ""

manageMinibatch :: Int -> Int -> Data -> [[Sample]] -> Data
manageMinibatch x (x - 1) network _ = generateBasedOf network
manageMinibatch amount counter network minibatches = let changes = minibatchEvaluation (minibatches !! counter) amount
                                                     in plus (plus network changes) (manageMinibatch amount (counter - 1) network minibatches)

-- TODO
minibatchEvaluation :: [Sample] -> Data
minibatchEvaluation minibatch = Data [[]] [] [[]] []

-- TODO
backpropagation :: Data -> Int -> Data
backpropagation network expectedResult = Data [[]] [] [[]] []

testEpoch :: [Sample] -> Int
testEpoch _ = 5
