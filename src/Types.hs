module Types
(Data (..) 
) where

data Data = Data {
                wHidden :: [[Float]],
                bHidden :: [Float],
                wOutput :: [[Float]],
                bOutput :: [Float]
                } deriving (Eq)
