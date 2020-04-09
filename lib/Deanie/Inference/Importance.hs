
module Deanie.Inference.Importance (
    importance
  ) where

import qualified Control.Foldl as L

importance
  :: (Foldable f, Applicative m, Floating w)
  => f a -> m b -> (b -> a -> w)
  -> m (w, b)
importance obs prior model =
  let parameter = prior
      cost = L.fold <$> (L.premap <$> (model <$> parameter) <*> pure L.sum) <*> pure obs
   in (,) <$> (exp <$> cost) <*> parameter
