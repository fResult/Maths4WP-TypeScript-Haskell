twice :: (a -> a) -> a -> a
twice f x = f (f x)

thrice :: (a -> a) -> a -> a
thrice f x = f (f (f x))

twice' :: (a -> a) -> a -> a
twice' f x = (f.f) x

thrice' :: (a -> a) -> a -> a
thrice' f x = (f.f.f) x
