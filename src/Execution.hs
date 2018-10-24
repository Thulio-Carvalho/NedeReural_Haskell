-- Modulo destinado a todas as funcoes relativas a execucao da Rede.

module Execution
(execute,
 initialize,
 feedforward) where

import InputOutput
import Types

type Image = [Float]
type Sample = (Int, Image)

-- execute, por retornar IO, pode interagir chamando outras IO Actions como
-- leitura dos weights, biases e input da Rede, por exemplo.
execute :: IO String -- mero esqueleto da funcao de execucao
execute = return ""

-- inicializa a rede com dados previamente salvos,
-- ou aleatorios em caso de primeira execucao.
initialize :: Data
initialize = Data [[]] [] [[]] []

feedforward :: [Float] -> Data
feedforward input = Data [[]] [] [[]] []