import Data.List
import Data.Char

main :: IO()
main = do   
    print "Who is the email for?"   
    recipient <- getLine   
    print "What is the Title?"   
    title <- getLine   
    print "Who is the Author?"   
    author <- getLine   
    print (createEmail recipient title author)

toPart recipient = "Dear " ++ recipient ++",\n"
bodyPart bookTitle = "Thanks for buying " ++ bookTitle ++ ".\n"
fromPart author = "Thanks,\n"++author

createEmail recipient bookTitle author = toPart recipient ++
                                         bodyPart bookTitle ++
                                         fromPart author

calcChange owed given = if change > 0
                        then change else 0
    where change = given - owed

inc x = x + 1
double x = x*2
square x = x^2

ifEven customFunction x = if even x
                          then customFunction x
                          else x

ifEvenInc = ifEven (\x -> x + 1)
ifEvenDouble = ifEven (\x -> x*2)
ifEvenSquare = ifEven (\x -> x^2)

sumSquareOrSquareSum x y = (\sumSquare squareSum ->
                            if sumSquare > squareSum
                            then sumSquare
                            else squareSum) (x^2 + y^2) ((x+y)^2)

overwrite x = (\x -> 
              (\x -> 
                  (\x -> x) 4
                  ) 3
                ) 2

counter x = let x = x + 1
            in
                let x = x + 1
                in
                    x

lambdaCounter x = (\x ->
                  (\x -> 
                    (\x -> x) x
                  ) x + 1
                ) x + 1

lambdaCounter2 x = (\x -> x + 1)
                   ((\x -> x + 1)
                    ((\x -> x) x))

names = [("Ian", "Curtis"),         
        ("Bernard","Sumner"),
        ("Peter", "Hook"),
        ("Stephen","Morris")]

compareLastNames name1 name2 = 
    if lastName1 /= lastName2
    then compare lastName1 lastName2
    else compare firstName1 firstName2
    where lastName1 = snd name1
          lastName2 = snd name2
          firstName1 = fst name1
          firstName2 = fst name2

sfOffice name = if lastName < "L"
                then nameText
                    ++ " - PO Box 1234 - San Francisco, CA, 94111"
                else nameText
                    ++ " - PO Box 1010 - San Francisco, CA, 94109"
    where lastName = snd name
          nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789 - New York, NY, 10013"
    where nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ " - PO Box 456 - Reno, NV 89523"
    where nameText = snd name

getLocationFunction location = case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))      

addressLetter name location = locationFunction name
    where locationFunction = getLocationFunction location     

getRequestURL host apiKey resource id = host ++ 
                                        "/" ++
                                        resource ++ 
                                        "/" ++
                                        id ++
                                        "?token=" ++
                                        apiKey

genHostRequestBuilder host = (\apiKey resource id -> 
                             getRequestURL host apiKey resource id)   

exampleUrlBuilder = genHostRequestBuilder "http://example.com"

genApiRequestBuilder hostBuilder apiKey resource = (\id ->
                                            hostBuilder apiKey 
                                            resource id)

superUrlBuilder = genApiRequestBuilder exampleUrlBuilder "1337hAsk3ll" "resource"

hyperBuilder = getRequestURL "http://example.com" "1337hAsk3ll" "bookk"

flipBinaryArgs binaryFunction = (\x y -> binaryFunction y x)

addressLetterV2 = flipBinaryArgs addressLetter
addressLetterNY = addressLetterV2 "ny"

subtract2 = flip(\x y -> x - y) 2

ssubtract2 = flip (-) 2

binaryPartialApplication binaryFunction x = (\y -> binaryFunction x y)
example x = binaryPartialApplication (-) x

reRepeat x = cycle[x]

subseq x y list = take (y - x) (drop x list)

inFirstHalf x list = elem x firstHalf
    where midPoint = div (length list) 2
          firstHalf = take midPoint list

myGCD a b = if remainder == 0
            then b
            else myGCD b remainder
    where remainder = a `mod` b

myTail (_:xs) = xs
myTail [] = []

superGCD x 0 = x
superGCD x y = superGCD y (x `mod` y)

myDrop 0 list = list
myDrop x list = myDrop (x-1) (tail list)

myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = x : rest
    where rest = myTake (n-1) xs

myCycle (first:rest) = first : myCycle(rest++[first])

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m-1) 1
ackermann m n = ackermann (m-1) (ackermann m (n-1))

collatz 1 = 1
collatz n = if even n
            then 1 + collatz(n `div` 2)
            else 1 + collatz(n * 3 + 1)

recReverse [] = []
recReverse (x:[]) = [x]
recReverse (x:xs) = (recReverse xs) ++ [x]

fastFib _ _ 0 = 0
fastFib _ _ 1 = 1
fastFib _ _ 2 = 1
fastFib x y 3 = x + y
fastFib x y n = fastFib (x+y) x (n-1)
fib n = fastFib 1 1 n

addAnA [] = []
addAnA (x:xs) = ("a " ++ x) :addAnA xs

myMap f [] = []
myMap f (x:xs) = (f x):myMap f xs

myRemove test [] = []
myRemove test (x:xs) = if test x
                       then myRemove test xs
                       else x:myRemove test xs

myFoldl :: (x -> x -> x) -> x -> [x] -> x
myFoldl f init [] = init
myFoldl f init (x:xs) = myFoldl f newInit xs 
    where newInit = f init x

myElem x list = (length filteredList) /= 0
    where filteredList = filter (\elem -> elem == x) list

isPalindrome string = checkedString == reverse checkedString
    where checkedString = map (\x -> toUpper x) filteredString
          filteredString = (filter (\elem -> elem /= ' ') string)

harmonic n = foldl (+) 0 series
    where series = map (\x -> 1 / x) [1..n]

halve :: Integer -> Integer
halve n = div n 2

printDouble :: Int -> String
printDouble x = show (x*2)

type FirstName = String
type MiddleName = String
type LastName = String
data RgType = Pos | Neg
data ABOType = A | B | AB | O
getABOType (BloodType abo _) = abo
data BloodType = BloodType ABOType RgType
data Sex = Male | Female
data Patient = Patient { name :: Name
                       , sex :: Sex
                       , age:: Int
                       , height :: Int
                       , weight :: Int
                       , bloodType :: BloodType }

canDonateTo :: Patient -> Patient -> Bool
canDonateTo (Patient _ _ _ _ _ (BloodType O _)) _ = True
canDonateTo _ (Patient _ _ _ _ _ (BloodType AB _)) = True
canDonateTo (Patient _ _ _ _ _ (BloodType A _)) (Patient _ _ _ _ _ (BloodType A _)) = True
canDonateTo (Patient _ _ _ _ _ (BloodType B _)) (Patient _ _ _ _ _ (BloodType B _)) = True
canDonateTo _ _ = False

class Describable a where
    describe :: a -> String


cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n

data SixSidedDie = S1 | S2 | S3 | S4 | S5 | S6 deriving (Enum, Eq, Ord)
instance Show SixSidedDie where
    show S1 = "one"
    show S2 = "two"
    show S3 = "three"
    show S4 = "four"
    show S5 = "five"
    show S6 = "six"

data Name = Name (String, String) deriving (Show, Eq)
instance Ord Name where
    compare (Name (f1,l1)) (Name (f2,l2)) = compare (l1,f1) (l2,f2)

data Number = One | Two | Three | Four deriving (Enum, Show)
instance Eq Number where
    (==) num1 num2 = (fromEnum num1) == (fromEnum num2)
instance Ord Number where
    compare num1 num2 = compare (fromEnum num1) (fromEnum num2)

data FiveSidedDie = Side1 | Side2 | Side3 | Side4 | Side5 deriving (Show, Eq, Enum)

class (Eq a, Enum a) => Die a where
    roll :: Int -> a

instance Die FiveSidedDie where
    roll num = toEnum (n `mod` 5)