module Anim 
  ( Anim
  , class Animable, animate, isRunning, current, fromCont
  ) where

import Prelude
import Data.Time.Duration (Milliseconds)


class Animable a where
  animate   :: Milliseconds -> a -> a
  isRunning :: a -> Boolean


data Anim a
    = Static a
    | Animated a (Milliseconds -> Anim a)


derive instance functorAnim :: Functor Anim


instance applyAnim :: Apply Anim where
  apply (Static f) (Static x) = Static (f x)
  apply (Animated f contF) sX@(Static x) = Animated (f x) (\delta -> apply (contF delta) sX)
  apply sF@(Static f) (Animated x contX) = Animated (f x) (\delta -> apply sF (contX delta))
  apply (Animated f contF) (Animated x contX) = Animated (f x) (\delta -> apply (contF delta) (contX delta))


instance applicativeAnim :: Applicative Anim where
  pure = Static  


instance animAnimable :: Animable (Anim a) where
  animate _     a@(Static _)      = a
  animate delta (Animated _ cont) = cont delta

  isRunning (Static _ )    = false
  isRunning (Animated _ _) = true


current :: forall a . Anim a -> a
current (Static a) = a
current (Animated a _) = a


fromCont :: forall a . a -> (Milliseconds -> Anim a) -> Anim a
fromCont = Animated