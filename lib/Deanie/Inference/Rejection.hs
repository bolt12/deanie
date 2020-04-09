
module Deanie.Inference.Rejection (

    rejection
  , grejection
  ) where

import Deanie.Language
import qualified Deanie.Expr as D
import Control.Monad
import qualified Data.Foldable as F
import Control.Selective
import Control.Selective.Free
import Control.Applicative.Extended

grejection
  :: (Foldable f)
  => ([a] -> [b] -> Bool) -> f b -> Program c -> (c -> Program a) -> Program c
grejection predicate observed proposal model = loop where
  len  = length observed
  loop = do
    parameters <- proposal
    generated  <- D.product $ replicateA len (liftAp $ model parameters)
    if   predicate generated (F.toList observed)
    then return parameters
    else loop

rejection :: (Foldable f, Eq a) => f a -> Program b -> (b -> Program a) -> Program b
rejection = grejection (==)
