module Boolean where

-- NAND logic
data Boolean = T | F
  deriving (Eq, Show)

bNand :: Boolean -> Boolean -> Boolean
bNand T T = F
bNand _ _ = T

bAnd :: Boolean -> Boolean -> Boolean
bAnd x y = (x `bNand` y) `bNand` (x `bNand` y)

bOr :: Boolean -> Boolean -> Boolean
bOr x y = (x `bNand` x) `bNand` (y `bNand` y)

bNor :: Boolean -> Boolean -> Boolean
bNor x y
  = ((x `bNand` x) `bNand` (y `bNand` y))
    `bNand` ((x `bNand` x) `bNand` (y `bNand` y))

bNot :: Boolean -> Boolean
bNot x = x `bNand` x

bXor :: Boolean -> Boolean -> Boolean
bXor x y
  = (x `bNand` (x `bNand` y))
    `bNand` (y `bNand` (x `bNand` y))

bEquiv :: Boolean -> Boolean -> Boolean
bEquiv x y
  = ((x `bNand` (x `bNand` y)) `bNand` (y `bNand` (x `bNand` y)))
    `bNand` ((x `bNand` (x `bNand` y)) `bNand` (y `bNand` (x `bNand` y)))

bImplies :: Boolean -> Boolean -> Boolean
bImplies T F = F
bImplies _ _ = T
