{-# LANGUAGE GADTs #-}
module Instances where
import Tensor
import Data.List
import Data.Monoid

bracket :: [String] -> String
bracket ss = "(" <> intercalate " " ss <> ")"

instance Show a => Show (V a) where
  show (V a) = show a
  show (VS s) = show s

instance Show a => Show (T a) where
 show (TAdd l r) = bracket  ["+", show l, show r]
 show (TMul l r) = bracket  ["*", show l, show r]
 show (TIx t ixs) = bracket  ["!", show t, bracket [show ix|ix<-ixs]]
 show (TV a) = show a
 show (TContract ixs t) = bracket ["Σ", bracket [show ix|ix<-ixs], show t]
 show (TLT l r) = bracket ["<?", show l, show r]
 show (TEQ l r) = bracket ["=?", show l, show r]
 show (TITE b l r) = bracket ["if", show b, show l, show r]
