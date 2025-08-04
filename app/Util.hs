module Util where

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt n f xs =
  let (before, after) = splitAt n xs
   in case after of
        [] -> xs
        (y : ys) -> before ++ f y : ys
