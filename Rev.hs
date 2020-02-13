{-# LANGUAGE DeriveFunctor #-}
module Rev where
import Control.Monad
data State s a = State { runstate :: s -> (a, s) } deriving (Functor)

instance Monad (State s) where
  return x = State $ \s -> (x, s)
  sa >>= a2sb = State $ \s -> let (a, s') = runstate sa s; sb = a2sb a; (b, s'') = runstate sb s' in (b, s'')

instance Applicative (State s) where
  pure = return
  (<*>) = ap

data Tardis s a = Tardis { runtardy :: s -> (a, s) } deriving(Functor)


instance Monad (Tardis s) where
  return x = Tardis $ \s -> (x, s)
  sa >>= a2sb = Tardis $ \s -> let (a, s') = runtardy sa s; sb = a2sb a; (b, s'') = runtardy sb s' in (b, s'')

instance Applicative (Tardis s) where
  pure = return
  (<*>) = ap

newtype Ldif a = Ld (a->(a,a))

lCnst c = Ld (\z -> (c, 0.0))
lDvar x = Ld (\z -> (x, z))


llift f f' (Ld pp) =Ld (\n->let (p,pb)= pp eb;eb= (f' p)*n in (f p,pb))
