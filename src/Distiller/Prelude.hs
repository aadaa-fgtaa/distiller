module Distiller.Prelude ( module R, module Distiller.Prelude ) where

import Relude as R hiding
  ( Alt, some, many, uncons, ordNub, ordNubOn, hashNub, mapMaybe, filter, catMaybes
  , readFile, readFileText, readFileLText, writeFile, writeFileText, writeFileLText
  , appendFile, appendFileText, appendFileLText, withFile )
import Control.Arrow as R hiding ( first, second )
import Control.Monad.Catch as R
import Control.Monad.Cont as R
import Control.Monad.Except as R
import Control.Monad.Writer.CPS as R hiding ( Alt, pass )
import Data.Align as R
import Data.Foldable.WithIndex as R
import Data.Functor.WithIndex as R
import Data.Functor.WithIndex.Instances ()
import Data.Map.Strict as R ( findWithDefault, toAscList, toDescList, assocs )
import Data.Text.IO.Utf8 as R
import Data.These as R
import Data.Traversable as R ( for )
import Data.Traversable.WithIndex as R
import Generic.Data as R
import Generic.Data.Microsurgery as R
import Pipes as R ( ListT )
import Witherable as R
import Data.Map.Strict qualified as Map

type Via this oth = ProductSurgery (CopyRep oth) this
type Wrapping this f = ProductSurgery (OnFields f) this

type Hash k = (Eq k, Hashable k)

associate :: Ord k => [k] -> [a] -> Map k a
associate a = Map.fromList . zip a

altMap :: (Foldable t, Alternative f, Functor t) => (a1 -> f a2) -> t a1 -> f a2
altMap f = asum . fmap f

faltMap :: (Foldable t, Alternative f, Functor t) => t a1 -> (a1 -> f a2) -> f a2
faltMap = flip altMap

ffoldMap :: (Foldable t, Monoid c) => t a -> (a -> c) -> c
ffoldMap = flip foldMap

sameZipWithM_ :: (MonadPlus m, Foldable t, Semialign t) => (a -> b -> m c) -> t a -> t b -> m ()
sameZipWithM_ f xs ys = sequence_ $ alignWith (these (const empty) (const empty) f) xs ys

sameZipWithM :: (MonadPlus m, Traversable t, Semialign t) => (a -> b -> m c) -> t a -> t b -> m (t c)
sameZipWithM f xs ys = sequence $ alignWith (these (const empty) (const empty) f) xs ys
