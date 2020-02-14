{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
module Tensor where
-- | indexing.
import Control.Applicative
import Control.Monad
data V a = V a | VS String -- values: either concrete  | symbolic
deriving instance Functor V 
type I = V Int -- indexes are either concrete or symbolic values.

data T a where
  TAdd :: T a -> T a -> T a
  TMul :: T a -> T a -> T a
  TIx :: T a -> [I] -> T a
  TV :: V a -> T a
  TContract :: [I] -> T a -> T a -- contract along index
  TLT :: Show a => T a -> T a -> T Bool -- compare less than
  TEQ :: Show a => T a -> T a -> T Bool -- compare equal
  TITE :: T Bool -> T a -> T a -> T a -- if then else

var :: String -> V a; var = VS
tconst :: a -> T a; tconst = TV . V
tsym :: String -> T a; tsym = TV . var

(.<) :: Show a => T a -> T a -> T Bool; (.<) = TLT
(.==) :: Show a => T a -> T a -> T Bool; (.==) = TEQ
(.!) :: T a -> [I] -> T a; (.!) = TIx


-- pred .? x .: y
-- can setup precedences so this parses as
-- (.:) (.? pred x) y
(.:) :: (T a -> T a) -> T a -> T a; (.:) predx y = predx y
(.?) :: T Bool -> T a -> T a -> T a; (.?) = TITE

instance Num a => Num (T a) where
  (+) = TAdd
  (*) = TMul
  negate x = TMul (fromInteger (-1)) x
  fromInteger = TV . V . fromInteger
  abs = error "unimplemented abs for T"
  signum = error "unimplemented signum for T"


-- M = monad
data M a where
 MR :: a -> M a -- return
 MF :: (a -> b) -> M a -> M b -- fmap
 MS :: (T Float -> M a) -> M a -- sample
 MD :: Num a => M a -> M a -> (T a -> M b) -> M b -- derivative

instance Functor M where
 fmap = MF
instance Applicative M where
 pure = return; (<*>) = ap
instance Monad M where
  return = MR
  (MR a) >>= f = f a
  (MF a2b ma) >>= f = do a <- ma; f (a2b a)
  (MS s2a) >>= f = MS (s2a >=> f)
  (MD n d d2b) >>= f = MD n d (d2b >=> f)

-- | compile monad
data CM a = CM { runcm :: Int -> (Int, a) }

cmuniq :: CM Int; cmuniq = CM $ \i -> (i+1,i)

runcompiler :: CM a -> a; runcompiler cm = let (_, a) = runcm cm 0 in a

instance Functor CM where
  fmap f (CM cm) = CM $ \i -> let (i', a) = cm i in (i', f a)
instance Applicative CM where
  pure = return; (<*>) = ap
instance Monad CM where
  return x = CM $ \i -> (i, x)
  (CM cm) >>= f = CM $ \i -> 
    let (i', a) = cm i; (i'', b) = runcm (f a) i' in (i'', b)

-- | TODO: create a new class called "DC a", which means one has
-- a differential calculus on that type. eg. finite differences.
compileder :: Num a => T a -> T a -> CM (T a)
compileder d@(TIx (TV (VS name)) ixs) e = return $ case e of
 TAdd l r -> TAdd (compileder d l) (compileder d r)
 TMul l r -> let l' = compileder d l; r' = compileder d r
    in TAdd (TMul l r') (TMul r' l)
 TIx (TV (VS name')) ixs' -> error "not done"
 TV _ -> 0
 TContract ixs t' -> TContract ixs (compileder d t')
 TLT l r ->  0 -- | good question, WTF?
 TEQ l r -> 0 -- | WTF?
 TITE l r -> 0 -- | WTF?
 _ -> 0 -- anything else, it's a straight 0
compileder (TV (V _)) _ = 0
compileder n d = error $ "unknown derivative"

-- | compile the monad into a large expression
compileM :: M a -> CM (T a)
compileM (MR t) = return (TV $ V t)
compileM (MS fk) = do
  i <- cmuniq; let v = TV (VS (show i))
  compileM $ fk v
compileM (MD n d derk) = do
  n <- compileM n; d <- compileM d
  der <- compileder n d
  compileM $ derk der

-- | now build manifolds. Literally all the data we need is the boundary maps
-- | the boundary maps are tensors. the nth boundary map is (n+1) -> n
-- | Mf[0] : 1 -> 0 ; Mf[1] : 2 -> 1 ; and so on
-- | the tensor encoding of the manifold
data Mft = Mf { unmf :: [T Int] }

-- | a form is defined on dimension 'n'. everything else is just there to keep it together.
-- | the tensor encoding of the differential form, which is 'just' an array, with a value at each point.
data Ft a = Form { unft :: [T a] }
