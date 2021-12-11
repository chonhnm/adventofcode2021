module Test where 


class MyFunctor f where 
    fmap' :: (a->b) -> f a -> f b 

instance MyFunctor (Either e) where
    -- fmap :: (a -> b) -> Either e a -> Either e b
    fmap' f (Left e) = Left e 
    fmap' f (Right a) = Right $ f a    


instance MyFunctor ((->) e) where 
    -- fmap :: (a -> b) -> (->) e a -> (->) e b 
    fmap' f g = f . g 

instance MyFunctor ((,) e) where 
    fmap' f (e,a) = (e, f a)

data Pair a = Pair a a 

instance Functor Pair  where 
    fmap f (Pair a b) = Pair (f a) (f b)       


data ITree a = Leaf (Int -> a) 
             | Node [ITree a]    


instance MyFunctor ITree where 
    fmap' f (Leaf g) = Leaf (f . g)
    fmap' f (Node xs) = Node (map (fmap' f) xs)             