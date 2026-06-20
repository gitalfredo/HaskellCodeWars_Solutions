one   f x =       f x                       -- :: (a -> b) -> a -> b
two   f x =    f (f x)   -- = f (one f x)   -- :: (a -> a) -> a -> a
three f x = f (f (f x))  -- = f (two f x)   -- :: (a -> a) -> a -> a

next :: ((b -> c) -> a -> b) -> (b -> c) -> a -> c
-- three f x = next two f x = f (two f x)   -- `two` is a formal parameter
--                            ~~~~~~~~~~~
next                num f x = f (num f x)   -- generic name `num`

zero :: t -> a -> a
zero f x = x

plus :: (t -> b -> c) -> (t -> a -> b) -> t -> a -> c
plus two one f x = two f (one f x)   -- formal parameters two, one
              -- = f (f (one f x))   -- an example substitution
              -- = f (f (f x)        --   uses the global definitions
              -- = three f x         --   for one, two, three