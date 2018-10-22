module Types
(Data (..),
 plus,
 divide,
 plusV,
 plusM,
 divideV,
 divideM
) where

data Data = Data {
                wHidden :: [[Float]],
                bHidden :: [Float],
                wOutput :: [[Float]],
                bOutput :: [Float]
                } deriving (Eq, Show)

-- Efetua soma entre os respectivos vetores e matrizes de ambos os Datas
plus :: Data -> Data -> Data
plus (Data wH1 bH1 wO1 bO1) (Data wH2 bH2 wO2 bO2) = (Data (plusM wH1 wH2) (plusV bH1 bH2) (plusM wO1 wO2) (plusV bO1 bO2))

-- Efetua divisao por esacalar entre os respectivos vetores e matrizes de ambos os Datas
divide :: Data -> Float -> Data
divide (Data wH bH wO bO) constant = (Data (divideM wH constant) (divideV bH constant) (divideM wO constant) (divideV bO constant))

-- Efetua soma entre dois vetores
plusV :: (Num a) => [a] -> [a] -> [a]
plusV list1 list2 = zipWith (+) list1 list2

-- Efetua soma entre duas matrizes
plusM :: (Num a) => [[a]] -> [[a]] -> [[a]]
plusM matrix1 matrix2 = zipWith plusV matrix1 matrix2

-- Efetua divisao por escalar em um dado vetor
divideV :: (Num a, Fractional a) => [a] -> a -> [a]
divideV list constant = map (/constant) list

-- Efetua divisao por escalar em uma dada matriz
divideM :: (Num a, Fractional a) => [[a]] -> a -> [[a]]
divideM matrix constant = map (\list -> divideV list constant) matrix