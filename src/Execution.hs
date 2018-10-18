-- Modulo destinado a todas as funcoes relativas a execucao da Rede.

module Execution
(execute) where

import InputOutput

-- execute, por retornar IO, pode interagir chamando outras IO Actions como
-- leitura dos weights, biases e input da Rede, por exemplo.
execute :: IO String -- mero esqueleto da funcao de execucao
execute = return ""
