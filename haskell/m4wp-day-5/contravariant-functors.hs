import Data.Char (isAlpha)
import Data.Functor.Contravariant (
  Contravariant (contramap),
  Predicate (..),
 )
import Data.List (init)
import Data.Text (Text, pack, splitOn, unpack)

main1 :: IO ()
main1 =
  let
    isAlphabet :: Predicate Char
    isAlphabet = Predicate isAlpha
   in
    do
      putStrLn $ "head of isAlphabet of ['1', 'a']: " ++ show (getPredicate (contramap head (Predicate isAlpha)) ['1', 'a'])
      putStrLn $ "head of isAlphabet of ['a', '1']: " ++ show (getPredicate (contramap head (Predicate isAlpha)) ['a', '1'])

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
csv2StringTuple csv = list2Tuple csvAsList
 where
  -- list2Tuple :: (Read a, Show b) => b -> a
  -- list2Tuple xs = read ("(" ++ (show xs :: String) ++ ")" :: String)
  list2Tuple :: forall {a1} {a2}. (Read a1, Show a2) => a2 -> a1
  list2Tuple xs = read $ "(" ++ (init . tail . show) xs ++ ")"
  csvAsList :: [String]
  csvAsList = map unpack $ splitOn (pack ",") (pack csv)

csv2PersonTuple :: String -> PersonTuple
csv2PersonTuple csv = (n, ln, a)
 where
  (n, ln, strA) = csv2StringTuple csv
  a = read strA :: Age

fromTuple :: MkPerson PersonTuple
fromTuple = MkPerson tuple2Person

-- λ > mkPerson fromTuple ("Korn", "Zilla", 18)
---- Person {name = "Korn", lastName = "Zilla", age = 18}

fromCsvString :: MkPerson String
fromCsvString = contramap csv2PersonTuple fromTuple

-- λ > mkPerson fromCsvString "Korn,Zilla,18"
---- Person {name = "Korn", lastName = "Zilla", age = 18}

data SimplePerson = SimplePerson Name LastName Age deriving (Show)

simplePerson2Tuple :: SimplePerson -> PersonTuple
simplePerson2Tuple (SimplePerson n ln a) = (n, ln, a)

fromSimplePerson :: MkPerson SimplePerson
fromSimplePerson = contramap simplePerson2Tuple fromTuple

-- λ > mkPerson fromSimplePerson (SimplePerson "Korn" "Zilla" 18)
---- Person {name = "Korn", lastName = "Zilla", age = 18}

main2 :: IO ()
main2 =
  let
    csvKorn :: String
    csvKorn = "Korn,Zilla,18"
    tupleKorn :: PersonTuple
    tupleKorn = ("Korn", "Zilla", 18)
    simpleKorn :: SimplePerson
    simpleKorn = SimplePerson "Korn" "Zilla" 18
   in
    do
      print "======= Prepare formats ======="
      putStrLn $ "CSV Korn: " ++ show csvKorn
      putStrLn $ "Tuple Korn: " ++ show tupleKorn
      putStrLn $ "Simple Korn: " ++ show simpleKorn
      print "======= Person from various formats ======="
      putStrLn $ "Person from Tuple: " ++ show (mkPerson fromTuple tupleKorn)
      putStrLn $ "Person from SimplePerson: " ++ show (mkPerson fromSimplePerson simpleKorn)
      putStrLn $ "Person from CSV: " ++ show (mkPerson fromCsvString csvKorn)

{--@@@@@ POC @@@@@--}
-- λ > :t read
---- read :: Read a => String -> a

-- λ > read "12" :: Int
---- 12

-- λ > read $ show "12" :: String
---- "12"

-- λ > read $ show "Korn" :: String
---- "Korn"

-- λ > read "False" :: Bool
---- False

-- λ > (read :: String -> Int) "123"
--- 123

-- λ > (read :: String -> String) (show "Korn")
---- "Korn"

-- λ > (read :: String -> Bool) "True"
---- True
