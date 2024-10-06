import Data.Char (toUpper, toLower)
-- {-# LANGUAGE ExistentialQuantification #-}
-- {-# LANGUAGE RankNTypes #-}
-- {-# LANGUAGE TypeSynonymInstances #-}

type Wrd = String

capitalizeWord :: Wrd -> Wrd
capitalizeWord [] = []
capitalizeWord (h:t) = toUpper h : fmap toLower t

capitalizeEachWords :: [Wrd] -> [Wrd]
capitalizeEachWords = fmap capitalizeWord

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x
