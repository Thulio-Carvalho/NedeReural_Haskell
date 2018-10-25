-- Modulo destinado a todas as funcoes relativas ao treino da Rede.

module Training 
(train) where

import Execution
import InputOutput
import Types
import Data.List.Split
import System.Random
import System.Random.Shuffle

type Image = [Float]
type Sample = (Int, Image)

train :: Int -> IO String
train epochAmount = do 
                let trainingSet = readTrainingSet
                    network = initialize
                    testSet = readTestSet
                manageTrainingEpoch epochAmount trainingSet testSet network
                -- save network

manageTrainingEpoch :: Int -> [Sample] -> [Sample] -> Data -> IO String
manageTrainingEpoch 0 _ _ _ = return ""
manageTrainingEpoch epochAmount trainingSet testSet network = do
                                                newNetwork <- trainingEpoch trainingSet network
                                                let 
                                                    correctCnt = testEpoch testSet newNetwork
                                                    totalAmount = length testSet
                                                do  return $ printEpoch epochAmount correctCnt totalAmount
                                                    manageTrainingEpoch (epochAmount - 1) trainingSet testSet newNetwork

trainingEpoch :: [Sample] -> Data -> IO Data
trainingEpoch trainingSet network = do
                                standardGenerator <- getStdGen
                                let trainingSize = length trainingSet
                                    shuffledTrainingSet = shuffle' trainingSet trainingSize standardGenerator
                                    minibatchAmount = 20 -- adequar quantidade
                                    minibatchSize = trainingSize `div` 20 -- adequar quantidade
                                    minibatches = chunksOf minibatchSize shuffledTrainingSet
                                    newNetwork = manageMinibatch minibatchAmount 0 network minibatches
                                return newNetwork

manageMinibatch :: Int -> Int -> Data -> [[Sample]] -> Data
manageMinibatch amount counter network minibatches = if amount /= (counter + 1) 
                                                        then  
                                                          let changes = minibatchEvaluation (minibatches !! counter) amount network
                                                          in plus (plus network changes) (manageMinibatch amount (counter + 1) network minibatches)
                                                        else
                                                          generateBasedOf network 

minibatchEvaluation :: [Sample] -> Int -> Data -> Data
minibatchEvaluation minibatch amount network = let sumedDesiredChanges = manageSample minibatch amount network
                                                   averageDesiredChanges = divide sumedDesiredChanges (fromIntegral amount)
                                               in averageDesiredChanges

manageSample :: [Sample] -> Int -> Data -> Data
manageSample minibatch counter networkModel = if counter > 0
                                    then
                                        let network = feedforward (snd $ minibatch !! counter) networkModel
                                            expectedOutput = buildExpectedOutput (fst $ minibatch !! counter)
                                            desiredChanges = backpropagation network expectedOutput
                                            sumChanges = generateBasedOf networkModel
                                        in plus (plus sumChanges desiredChanges) (manageSample minibatch counter networkModel)
                                    else
                                        generateBasedOf networkModel

buildExpectedOutput :: Int -> [Float]
buildExpectedOutput representedValue = let indexes = [0.0 .. 9.0]
                                       in [if x == (fromIntegral representedValue) then x else 0.0 | x <- indexes]

-- Recebe as informacoes da rede neural, o resultado 
-- esperado e retorna um Data com as modificacoes necessarias
-- na rede
-- N = network, E = expected list
backpropagation :: Data -> [Float] -> Data
backpropagation n e
    | isEmpty n = error "Backpropagation error: Data is empty"
    | length e /= 10 = error"Backpropagation error: expectedOutput list is invalid"
    | otherwise = let outputError = computeOutputError (aOutput n) e (zetaOutput n)
                  in n

-- Gera o vetor de erro da camada output, recebe
-- as ativacoes do output atual, as ativacoes esperadas 
-- e a lista zeta do output
computeOutputError :: [Float] -> [Float] -> [Float] -> [Float]
computeOutputError a e z = hadamardV a (sigV' z)

testEpoch :: [Sample] -> Data -> Int
testEpoch testSet network = manageEpoch testSet network (length testSet)

manageEpoch :: [Sample] -> Data -> Int -> Int
manageEpoch testSet network counter = if counter > 0
                                        then
                                            let newNetwork = feedforward (snd $ testSet !! counter) network
                                            in if (aOutput newNetwork) == buildExpectedOutput (fst $ testSet !! counter) 
                                                then
                                                    1 + manageEpoch testSet network (counter - 1)
                                                else
                                                    0 + manageEpoch testSet network (counter - 1)
                                        else
                                            0
                                            