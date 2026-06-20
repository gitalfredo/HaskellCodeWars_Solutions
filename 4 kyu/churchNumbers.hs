-- TODO
{-# LANGUAGE RankNTypes #-}
import Prelude hiding (succ)

newtype Number = Nr (forall a. (a -> a) -> a -> a) 

zero :: Number
zero = Nr (\ _ z -> z)

succ :: Number -> Number
succ (Nr a) = Nr (\ f b -> f (a f b))

one :: Number
one = succ zero 

two :: Number
two = succ (succ zero) 

three :: Number
three = succ (succ (succ zero))

churchToNumber :: Number -> Int
churchToNumber (Nr a) = a (+1) 0

add :: Number -> Number -> Number
add (Nr m) (Nr n) = Nr (\ f z -> (n f) (m f z))

mult :: Number -> Number -> Number
mult (Nr m) (Nr n) = Nr (\ f z -> (m (n f) z))

pow :: Number -> Number -> Number
pow (Nr m) (Nr n) = Nr (\ f z -> ((n m) f) z)

-- Tests
-- churchToNumber (succ zero) is (\ f b -> f ((\ _ z -> z) f b)) (+1) 0