module Crispy where

newtype P = P Int
  deriving (Eq, Show)

newtype C = C Int
  deriving (Eq, Show)

data Di a = Di a a
  deriving (Eq, Show)

encryptUsing :: (P -> C) -> P -> C
encryptUsing f = f

decryptUsing :: (C -> P) -> C -> P
decryptUsing g = g

shift :: Int -> P -> C
shift k (P i) = C ((i + k) `mod` 26)

unshift :: Int -> C -> P
unshift k (C i) = P ((i - k) `mod` 26)

group :: Di P -> P
group (Di (P x) (P y)) = P $ (x * 26) + y

multiplyAndShift :: Int -> Int -> P -> C
multiplyAndShift = undefined
