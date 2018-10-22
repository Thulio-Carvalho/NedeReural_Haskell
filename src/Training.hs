-- Modulo destinado a todas as funcoes relativas ao treino da Rede.

module Training 
(train) where

import Execution
import InputOutput
import Types

type Image = [Float]
type Sample = (Int, Image)

-- execute, por retornar IO, pode interagir chamando outras IO Actions como
-- leitura dos weights, biases e traning sets, por exemplo.
train :: Int -> IO String -- mero esqueleto da funcao de treino
train epochAmount = manageEpoch epochAmount trainingSets testSet network
                    where   trainingSets = readTrainingSet
                            network = initialize
                            testSet = readTestSet

manageEpoch :: Int -> [Sample] -> [Sample] -> Data -> IO String
manageEpoch 0 _ _ _ = return ""
manageEpoch epochAmount trainingSets testSet network = do
                                                let correctCnt = testEpoch testSet
                                                trainingEpoch trainingSets
                                                printEpoch epochAmount correctCnt (length testSet)
                                                manageEpoch (epochAmount - 1) trainingSets testSet network

                                                


-- quantidade constante de minibatches
-- tamanho constante de minibatches
trainingEpoch :: [Sample] -> IO String
trainingEpoch _ = return ""




testEpoch :: [Sample] -> Int
testEpoch _ = 5