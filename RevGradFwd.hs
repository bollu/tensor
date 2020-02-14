{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveFunctor #-}
import Control.Monad
import Control.Monad.Fix

data T fix f bix b a = T { runt :: (fix -> f, bix -> b) -> (a, fix -> f, bix -> b) } deriving(Functor)
instance Applicative (T fix f bix b) where pure = return; (<*>) = ap
instance Monad (T fix f bix b) where 
  return x = T $ \ ~(f, b) -> (x, f, b)
  tx >>= x2ty = T $ \ ~(f, b) -> 
   let ~(x, f', b'') = runt tx (f, b')
       ty = x2ty x
       ~(y, f'', b') = runt ty (f', b)
    in (y, f'', b'')

instance MonadFix (T fix f bix b) where 
   mfix f = do
      rec x <- (f x)
      return x

setfwdval :: Eq fix => fix -> f -> T fix f bix b ()
setfwdval vi fv = T $ \ ~(f, b) -> ((), \i -> if i == vi then fv else f i, b)

setbwdval :: Eq bix => bix -> b -> T fix f bix b ()
setbwdval vi bv = T $ \ ~(f, b) -> ((), f, \i -> if i == vi then bv else b i)

getbwdval :: bix -> T fix f bix b b
getbwdval bi = T $ \ ~(f, b) -> (b bi, f, b)

getfwdval :: fix -> T fix f bix b f
getfwdval fi = T $ \ ~(f, b) -> (f fi, f, b)


type Name = String
type I x = T Name Int Name Int x

-- V1.
-- | values flow backward, gradients flow forward
getvar :: Name -> I Int
getvar n = getbwdval n

-- | set value
setvar :: Name -> Int -> I ()
setvar n i = setbwdval n  i

getgrad :: Name -> I Int
getgrad n = getfwdval n

setgrad :: Name -> Int -> I ()
setgrad n i = setfwdval n i

accumgrad :: Name -> Int -> I ()
accumgrad n i = do
  g <- getgrad n
  setgrad n (i + g)

-- x = 10
-- xsq = x * x
p1 :: I Int
p1 = mdo
 setgrad "xsq" 1
-- xsq code
 l <- getvar "x"; r <- getvar "x"
 setvar "xsq" (l * r)
 d_xsq <- getgrad "xsq"
 accumgrad "x" (d_xsq * r)
 accumgrad "x" (d_xsq * l)
-- x code
 setvar "x" 10
 return $ l

(v, grads, vals) =  runt p1 (const 0, const 0)
runthis = (vals "x", vals "xsq", grads "x", grads "xsq")