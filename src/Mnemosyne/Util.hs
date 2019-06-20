module Mnemosyne.Util where

import Data.Function (on)
import Data.List (groupBy)

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

intersperseM_ :: (Monad m) => m a -> [m a] -> m ()
intersperseM_ _ [] = return ()
intersperseM_ a (m:ms) = m >> mapM_ (\x -> a >> x) ms
