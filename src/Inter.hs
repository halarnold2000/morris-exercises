module Inter where

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

-- | tests how to do fmap
-- >>> fmap' (3*) [1,2,3]
-- [3,6,9]

-- Relative Difficulty: 1
instance Functor' [] where
  fmap' f [] = []
  fmap' f (x:xs) = f x : fmap' f xs

-- | test: maybe for fmap'
-- >>> fmap' (3+) (Just 3)
-- Just 6

-- Exercise 2
-- Relative Difficulty: 1
instance Functor' Maybe where
  fmap' f Nothing = Nothing
  fmap' f (Just a) = Just (f a)

-- Exercise 3
-- Relative Difficulty: 5

-- | test: (->) r for fmap'
-- >>> fmap' (3+) (*4) 6
-- 27
instance Functor' ((->) t) where
  fmap' = (.)

-- this shows that Either could be either left or right convention

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Functor' (EitherLeft t) where
  fmap' f (EitherLeft (Left x)) = EitherLeft (Left (f x))
  fmap' f (EitherLeft (Right x))= EitherLeft (Right x)

-- Exercise 5
-- Relative Difficulty: 5
instance Functor' (EitherRight t) where
  fmap' f (EitherRight (Right a)) = EitherRight (Right (f a))
  fmap' f (EitherRight (Left x)) = EitherRight (Left x)
  -- fmap' f (EitherRight z) = EitherRight z
  --fmap' f x = x


class Functor' m => Monad' m where
  bind' :: (a -> m b) -> m a -> m b
  bind'' :: m a -> (a -> m b) -> m b
  return' :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use bind' and/or return')
  --fmap'' :: (a -> b) -> m a -> m b

  -- x = ma
  --fmap'' f  = bind' $ return' . f
  bind'' = flip bind'

-- |
--
-- Exercise 7
-- Relative Difficulty: 2
instance Monad' [] where
  bind' f x = concat (fmap' f x)
  return' x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Monad' Maybe where
  bind' f Nothing = Nothing
  bind' f (Just x)= f x
  return' = Just

-- Exercise 9
-- Relative Difficulty: 6
instance Monad' ((->) t) where
  bind' f x = \y -> f (x y) y
  return' = const

-- Exercise 10
-- Relative Difficulty: 6
instance Monad' (EitherLeft t) where
  bind' f (EitherLeft (Left x)) = f x
  bind' f (EitherLeft (Right x))= EitherLeft (Right x)
  return' x = EitherLeft (Left x)

-- Exercise 11
-- Relative Difficulty: 6
instance Monad' (EitherRight t) where
  bind' f (EitherRight (Right x)) = f x
  bind' f (EitherRight (Left y))  = EitherRight (Left y)
  return' x = EitherRight (Right x)

-- Exercise 12
-- Relative Difficulty: 3
join' :: (Monad' m) => m (m a) -> m a
join' mma = bind' id mma

-- Exercise 13
-- Relative Difficulty: 6
apply' :: (Monad' m) => m a -> m (a -> b) -> m b

-- bind' :: (a - mb) -> ma -> mb
-- (\f -> fmap'' f ma)
-- apply' = bind' . flip fmap''
apply' ma = bind' $ flip fmap' ma

ap :: (Monad' m) => m (a -> b) -> m a -> m b
ap mf mx =
      bind'' mf  ( \f ->
      bind'' mx  ( \x ->
      return' (f x)  ))

--apply'' :: (Monad' m) => m (a ->b) -> m a -> m b
--apply'' ma = bind' $ fmap'' ma

-- Exercise 14
-- Relative Difficulty: 6
-- actually a 'forM'
forM' :: (Monad' m) => [a] -> (a -> m b) -> m [b]
forM' as a_to_mb = foldr fn (return' []) as
  where
    fn a mlb =
         bind' (\x ->
         bind' (\lb ->
         return' (x : lb )) mlb ) (a_to_mb a)

mapM' :: (Monad' m) => (a -> m b ) -> [a]  -> m [b]
mapM' a_to_mb  = foldr binary_fn (return' [])
  where
    binary_fn a mlb =
       bind'' (a_to_mb a) (\x ->
       bind'' mlb (\lb ->
       return'  (x : lb) ))

-- mb ---       bind' (\b -> do some stuff here) mb
-- mlb ---      bind' (\lb -> do some stuff with [b] here) mlb

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
-- (bonus: use forM)
seq':: (Monad' m) => [m a] -> m [a]
seq'  = mapM' id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use ap + fmap'') ap' and fmap''
-- <*> :: f (a->b) -> (f a -> f b)
lift2' :: (Monad' m) => (a -> b -> c) -> m a -> m b -> m c
lift2' f ma mb = f `fmap'` ma `ap` mb
-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use ap + lift2' )
lift3' :: (Monad' m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3' f ma mb mc = (lift2' f ma mb) `ap` mc

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use ap + lift3')
lift4' :: (Monad' m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
lift4' f ma mb mc = ap (lift3' f ma mb mc)

newtype State s a = State {
  state :: (s -> (s, a))
}


-- Exercise 19
-- aztecrex
-- Relative Difficulty: 9
-- fmap' :: (a ->b) -> f a - > f b
-- s -> (s, a)

-- apply the function f
--


instance Functor' (State s) where
  fmap' f ms = State $ \s -> let (s', a) = state ms s
                            in (s', f a)

-- Exercise 20
-- Relative Difficulty: 10
instance Monad' (State s) where
  bind' = error "todo"
  return' = error "todo"
