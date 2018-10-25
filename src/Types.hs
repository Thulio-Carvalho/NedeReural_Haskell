module Types
(Data (..),
 plus,
 divide,
 plusV,
 plusM,
 divideV,
 divideM,
 generateBasedOf,
 isEmpty,
 hadamardV,
 hadamardM
) where

data Data = Data {
                wHidden :: [[Float]],
                bHidden :: [Float],
                aHidden :: [Float],
                zetaHidden :: [Float],
                wOutput :: [[Float]],
                bOutput :: [Float],
                aOutput :: [Float],
                zetaOutput :: [Float]
                } deriving (Eq, Show)

-- Efetua soma entre os respectivos vetores e matrizes de ambos os Datas
plus :: Data -> Data -> Data
plus (Data wH1 bH1 aH1 zH1 wO1 bO1 aO1 zO1) (Data wH2 bH2 aH2 zH2 wO2 bO2 a02 z02) = 
    (Data (plusM wH1 wH2) (plusV bH1 bH2) aH1 zH1 (plusM wO1 wO2) (plusV bO1 bO2) aO1 zO1)

-- Efetua divisao por esacalar entre os respectivos vetores e matrizes de ambos os Datas
divide :: Data -> Float -> Data
divide (Data wH bH aH zH wO bO aO zO) constant = (Data (divideM wH constant) (divideV bH constant) aH zH (divideM wO constant) (divideV bO constant) aO zO)

-- Efetua soma entre dois vetores
plusV :: (Num a) => [a] -> [a] -> [a]
plusV list1 list2 = zipWith (+) list1 list2

-- Efetua soma entre duas matrizes
plusM :: (Num a) => [[a]] -> [[a]] -> [[a]]
plusM matrix1 matrix2 = zipWith plusV matrix1 matrix2

-- Efetua hadamard entre dois vetores
hadamardV :: (Num a) => [a] -> [a] -> [a]
hadamardV list1 list2 = zipWith (*) list1 list2

-- Efetua hadamard entre duas matrizes
hadamardM :: (Num a) => [[a]] -> [[a]] -> [[a]]
hadamardM matrix1 matrix2 = zipWith hadamardV matrix1 matrix2

-- Efetua divisao por escalar em um dado vetor
divideV :: (Num a, Fractional a) => [a] -> a -> [a]
divideV list constant = map (/constant) list

-- Efetua divisao por escalar em uma dada matriz
divideM :: (Num a, Fractional a) => [[a]] -> a -> [[a]]
divideM matrix constant = map (\list -> divideV list constant) matrix

sig :: Float -> Float
sig x = 1.0 - (1.0 + exp(-x))

sig' :: Float -> Float
sig' x = sig x * (1 - sig x) 

sigV :: [Float] -> [Float]
sigV l = [sig x | x <- l]

sigV' :: [Float] -> [Float]
sigV' l = [sig' x | x <- l]

-- Cria um data repleto de valores 0.0 com as mesmas dimensoes do modelo passado por parametro
generateBasedOf :: Data -> Data
generateBasedOf (Data wH bH aH zH wO bO aO zO) = let whRows = length wH
                                                     whColumns = length $ wH !! 0
                                                     bhLen = length bH
                                                     ahLen = length aH
                                                     zhLen = length zH
                                                     woRows = length wO
                                                     woColumns = length $ wO !! 0
                                                     boLen = length bO
                                                     aoLen = length aH
                                                     zoLen = length zH
                                                     
                                                     newWH = replicate whRows (replicate whColumns 0.0)
                                                     newBH = replicate bhLen 0.0
                                                     newAH = replicate ahLen 0.0
                                                     newZH = replicate zhLen 0.0
                                                     newWO = replicate woRows (replicate woColumns 0.0)
                                                     newBO = replicate boLen 0.0
                                                     newAO = replicate aoLen 0.0
                                                     newZO = replicate zoLen 0.0
                                                 in (Data newWH newBH newAH newZH newWO newBO newAO newZO)

isEmpty :: Data -> Bool
isEmpty (Data wH bH aH zH wO bO aO zO) = 
    (null wH) || (null bH) || (null wO) || (null bO)