-- Modulo destinado a todas as funcoes relativas ao treino da Rede.

module Training 
(train) where

import Execution
import InputOutput

-- execute, por retornar IO, pode interagir chamando outras IO Actions como
-- leitura dos weights, biases e traning sets, por exemplo.
train :: IO String -- mero esqueleto da funcao de treino
train = return ""