import Data.Functor.Contravariant
import Data.List (init)
import Data.Text (splitOn, Text, pack, unpack)

data Person = Person
  { name :: String
  , lastName :: String
  , age :: Int
  }
  deriving (Show)

newtype MkPerson a = MkPerson {mkPerson :: a -> Person}

instance Contravariant MkPerson where
  contramap :: (b -> a) -> MkPerson a -> MkPerson b
  contramap f (MkPerson a) = MkPerson (a . f)

type PersonTuple = (Name, LastName, Age)
type Name = String
type LastName = String
type Age = Int

tuple2Person :: (Name, LastName, Age) -> Person
tuple2Person (n, ln, a) = Person{name = n, lastName = ln, age = a}

csv2StringTuple :: String -> (String, String, String)
csv2StringTuple csv = list2Tuple csvAsList::(String, String, String) where
  list2Tuple xs = read $ "(" ++ (init.tail.show) xs ++ ")"
  csvAsList     = map unpack $ splitOn (pack ",") (pack csv)

csv2Tuple :: String -> PersonTuple
csv2Tuple csv = (n, ln, a)
  where
    (n, ln, sa) = csv2StringTuple csv
    a = read sa::Age

fromTuple :: MkPerson PersonTuple
fromTuple = MkPerson tuple2Person
-- λ > mkPerson fromTuple ("Korn", "Zilla", 18)
---- Person {name = "Korn", lastName = "Zilla", age = 18}

fromCsvString :: MkPerson String
fromCsvString = contramap csv2Tuple fromTuple
-- λ > mkPerson fromCsvString "Korn,Zilla,18"
---- Person {name = "Korn", lastName = "Zilla", age = 18}
