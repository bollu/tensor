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
p1 = do
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


-- | multiply two values
mul :: Name -> Name -> Name -> I ()
mul x nl nr = do
 l <- getvar nl; r <- getvar nr
 setvar x (l * r)
 ds_dx <- getgrad x
 accumgrad nl (ds_dx * r)
 accumgrad nr (ds_dx * l)

add :: Name -> Name -> Name -> I ()
add x nl nr = do
 l <- getvar nl; r <- getvar nr
 setvar x (l + r)
 ds_dx <- getgrad x
 accumgrad nl (ds_dx)
 accumgrad nr (ds_dx)

p2 :: I ()
p2 = do
 setgrad "z" 1

 -- z = xy + xsq
 xt <- getvar "xy"
 xsq <- getvar "xsq"
 setvar "z" (xt + xsq)
 ds_dz <- getgrad "z"
 accumgrad "xy" (ds_dz * 1)
 accumgrad "xsq" (ds_dz * 1)

 -- x * y
 x <- getvar "x"
 y <- getvar "y"
 setvar "xy" (x * y)

 ds_dxdy <- getgrad "xy"
 accumgrad "x" (ds_dxdy * y)
 accumgrad "y" (ds_dxdy * x)

 -- xsq
 x <- getvar "x"
 setvar "xsq" (x * x)
 ds_dxsq <- getgrad "xsq"
 accumgrad "x" (ds_dxsq * x)
 accumgrad "x" (ds_dxsq * x)

 setvar "x" 2
 setvar "y" 3

((), grads2, vals2) =  runt p2 (const 0, const 0)
runthis2 = (vals2 "x", vals2 "y", vals2 "xsq", vals2 "xy", vals2 "z", 
            grads2 "x", grads2 "y", grads2 "xsq", grads2 "xy", grads2 "z")
