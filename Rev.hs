{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RecursiveDo #-}

module Rev where
import qualified Control.Monad.Fail as MF
import Control.Monad
import Control.Monad.Fix
import qualified Data.Map.Strict as M
import Data.List
import Control.Applicative

data S s a = S { runs :: s -> (a, s) } deriving(Functor)
data R s a = R { runr :: s -> (a, s) } deriving (Functor)

gets :: S s s
gets = S $ \s -> (s, s)

puts :: s -> S s ()
puts s = S $ \_ -> ((), s)

modifys :: (s -> s) -> S s ()
modifys f = S $ \s -> ((), f s)


getr :: R r r
getr = R $ \r -> (r, r)

putr :: r -> R r ()
putr r = R $ \_ -> ((), r)

modifyr :: (r -> r) -> R r ()
modifyr f = R $ \r -> ((), f r)

instance Monad (S s) where
 return x = S $ \s -> (x, s)
 sx >>= x2sy = S $ \s -> let (x, s') = runs sx s; sy = x2sy x; (y, s'') = runs sy s' in (y, s'') 

instance Applicative (S s) where pure = return; (<*>) = ap

instance Monad (R s) where
 return x = R $ \s -> (x, s)
 sx >>= x2sy = R $ \s -> let (x, s'') = runr sx s'; sy = x2sy x; (y, s') = runr sy s in (y, s'') 

instance Applicative (R s) where pure = return; (<*>) = ap

type Name = String
type Env a = M.Map Name a

-- | instruction. build reverse mode AD
{-
data I a = I (S Env a) (R Env a) deriving(Functor)

instance Monad I where
  return x = I (return x) (return x)
  (I sx rx) >>= x2syry = I is ir where
    is = S $ \s -> let (x, s') = runs sx s; (I sy _) = x2syry x; (y, s'') = runs sy s' in (y, s'')
    ir = R $ \r -> let (x, r') = runr rx r; (I _ ry) = x2syry x; (y, r'') = runr ry r' in (y, r'')
instance Applicative I where pure = return; (<*>) = ap
     
getenv :: I (Env  Int)
getenv = I (S $ \s -> (s, s)) (R $ (\r -> (r, r)))


eval :: E -> I Int
eval (EC i) = return i
eval (EV n) = do
  env <- getenv
  return $ env M.! n
-}

-- x = y + z
-- ds/dx = ds/dy.dy/dx + ds/dz.dz/dx

data SRM a = SRM {
  run :: (Env Int, Env Int) -> (a, Env Int, Env Int)
} 

instance Functor SRM where
  fmap f mx = do
   x <- mx
   return $ f x

instance Monad SRM where
  return a = SRM $ \ ~(v, g) -> (a, v, g)
  sx >>= x2sy = SRM $ \ ~ (v, g)  -> 
    let ~(x, v', g'') = run sx (v, g'); sy = x2sy x; ~(y, v'', g') = run sy (v', g) in (y, v'', g'')

instance MonadFix SRM where
   -- mfix :: (x -> SRM x) -> SRM x
   mfix f = do
      rec x <- (f x)
      return x

instance Applicative SRM where pure = return; (<*>) = ap

accumgrad_ :: Name -> Int -> SRM ()
accumgrad_ n v = SRM $ \ ~(vals, ders) -> ((), vals, M.insertWith (+) n v ders)


accumval_ :: Name -> Int -> SRM ()
accumval_ n v = SRM $ \ ~(vals, ders) -> ((), M.insertWith (+) n v vals, ders)

recvgrad :: Name -> SRM Int
-- recvgrad n = SRM $ \vals ders -> (M.findWithDefault 0 n ders, vals, ders)
recvgrad n = SRM $ \ ~(vals, ders) -> (ders M.! n, vals, ders)

recvval :: Name -> SRM Int
recvval n = SRM $ \ ~(vals, ders) -> (vals M.! n, vals, ders)

sendval :: Name -> Int -> SRM ()
sendval n v = SRM $  \ ~(vals, ders) -> ((), M.insert n v vals, ders) 

sendgrad :: Name -> Int -> SRM ()
sendgrad n v = SRM $  \ ~(vals, ders) -> ((), vals, M.insert n v ders)

-- | values
data V = VC Int | VN Name deriving(Show)

evalV ::  V -> SRM Int
evalV (VC n) = return n
evalV (VN n) = recvval n

accumgrad :: V -> Int -> SRM ()
accumgrad (VC _) g = return ()
accumgrad (VN n) g = accumval_ n g

-- | send value backwards in time
var :: Name -> Int -> SRM ()
var n val = mdo
  sendgrad n val
  return ()

add :: Name -> V -> V -> SRM ()
add n l r = do
  il <- evalV l
  ir <- evalV r

  sendval n (il + ir)
  -- n = l + r
  -- ds/dn
  dsdn <- recvgrad n
  -- ds/dl = ds/dn . dn/dl ||| ds/dr = ds/dn . dn/dr
  accumgrad l (dsdn*1)
  accumgrad r (dsdn*1)


mul :: Name -> V -> V -> SRM ()
mul n l r = mdo
  -- n = l + r
  -- ds/dn
  dsdn <- recvgrad n

  il <- evalV l
  ir <- evalV r

  sendval n (il + ir)

  -- ds/dl = ds/dn . dn/dl ||| ds/dr = ds/dn . dn/dr
  accumgrad l (dsdn*ir)
  accumgrad r (dsdn*il)

  return ()

program0 :: SRM ()
program0 = var "z" 3

program1 :: SRM ()
program1 = do
 var "x" 2
 mul "xsq" (VN "x") (VN "x")


-- | contains x, ds_dx, sq, ds_dsq
-- sq = x * x
programflat :: (Int, Int, Int, Int)
programflat = (x, ds_dx, sq, ds_dsq) where
  x = 2
  sq = x * x
  ds_dsq = 1
  dsq_dl = ds_dsq * x
  dsq_dr = ds_dsq * x
  ds_dx = dsq_dl + dsq_dr

program2 :: SRM ()
program2 = do
 var "x" 2
 var "y" 3
 mul "xsq" (VN "x") (VN "x")
 add "z" (VN "xsq") (VN "y")


-- | x = 10
-- xsq = x * x
program_manual :: SRM ()
program_manual = mdo
  accumval_ "xsq" 1
  x <- recvgrad "x"
  sendgrad "xsq" (x * x)
  -- ds_dxsq <- recvval "xsq"
  -- accumval_ "xval" (xval)
  sendgrad "x" 10

runprogram :: SRM a -> Name -> (a, Env Int, Env Int)
runprogram p output = 
  let (a, vals, ders) = run (do v <- p; return v) (M.empty, M.empty)
  in (a, vals, ders)

