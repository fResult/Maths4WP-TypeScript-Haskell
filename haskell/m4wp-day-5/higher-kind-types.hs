-- Higher-kind type sometimes A.K.A a Function Constructor Type
-- Int :: *
---- Int is a type

-- Maybe :: * -> *
---- Maybe takes a type and returns a type

-- Maybe Bool :: *
---- Maybe takes a type `Bool` and returns a type `Maybe Bool :: *`

-- a -> a :: *
---- a -> a takes two types and returns a type

-- [] :: * -> *
---- [] takes a type and returns a type

-- [Int] :: *
---- [] takes a type `Int` and returns a type `[Int] :: *`

-- (->) :: * -> * -> *
---- (->) takes two types (in a curried form) and returns a type
