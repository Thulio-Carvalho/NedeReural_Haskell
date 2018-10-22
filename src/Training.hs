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
train :: Int -> IO () -- mero esqueleto da funcao de treino
train epochAmount = manageEpoch epochAmount trainingSets testSet network
                    where   trainingSets = readTrainingSet
                            network = initialize
                            testSet = readTestSet 

manageEpoch :: Int -> [Sample] -> [Sample] -> Data -> IO ()
manageEpoch 0 _ _ _ = return ()
manageEpoch epochAmount trainingSets testSet network = do
                                                trainingEpoch trainingSets
                                                printEpoch epochAmount correctCnt (length testSet)
                                                manageEpoch (epochAmount - 1) trainingSets testSet network
                                                where correctCnt = testEpoch testSet
                                                


-- quantidade constante de minibatches
-- tamanho constante de minibatches
trainingEpoch :: [Sample] -> ()
trainingEpoch _ = ()




testEpoch :: [Sample] -> Int
testEpoch _ = 5