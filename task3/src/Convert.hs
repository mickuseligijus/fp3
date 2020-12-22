module Convert
where

import Data.Char
import Data.List
import Data.Function
import Tictactoe

data JsonLikeValue = JLMap [(String, JsonLikeValue)] | Coordinates (Int,Int,Char) deriving (Show, Eq)

parse :: Int ->String-> Either String JsonLikeValue
parse _ message = case parseriukas message of
    Right (a,_) -> Right a
    Left e -> Left e

parseriukas :: String -> Either String (JsonLikeValue,String)
parseriukas message = case parse' message of
                        Right (JLMap value,rest) -> 
                            if rest /="" then
                                            case parseTillEnd rest [] of
                                                Right (JLMap val2,remnent)-> Right (JLMap $ value++val2,remnent)
                                                Right (_,_)-> Left "error in parseriukas"
                                                Left e -> Left e
                                                    else 
                                                        Right (JLMap value,rest)
                        Right (_,_)-> Left "error in parseriukas"
                        Left e -> Left e
                       
parseTillEnd :: String -> [(String,JsonLikeValue)]->Either String (JsonLikeValue,String)
parseTillEnd message acc = case parseJsonLikeValue message of
                            Right (JLMap val,rest) -> if rest /= ""
                                                        then parseTillEnd rest (acc++val)
                                                            else Right (JLMap val,rest)
                            Right (_,_)-> Left "error in parseTillEnd"
                            Left e -> Left e

parse' :: String -> Either String (JsonLikeValue,String)
parse' message  =
    case message of
        ('d':rest) -> case parseJsonLikeValue rest  of
                        Left e -> Left e
                        Right (JLMap value,liekana) -> if liekana /=""
                                                            then case parseJsonLikeValue liekana  of
                                                                    Right(JLMap v2,l2) -> Right (JLMap $ value++v2, consume l2) 
                                                                    Left e->Left e
                                                                    Right (_,_)-> Left "error in parse'"
                                                                else Right (JLMap value, liekana)
                        Right(_,_)-> Left "error in parse'"                                        
        _ -> Left "error in parse' "

parseJsonLikeValue :: String ->Either String (JsonLikeValue,String)
parseJsonLikeValue message  =
    case message of
    ('4':':':rest) -> case parseMap rest of
                        Right (value,r) -> Right (value,r)
                        Left e -> Left e
    ('e':rest) -> Right (JLMap [],rest)
    _ -> Left $ "parseJsonLikeValue error "

parseMap :: String -> Either String (JsonLikeValue,String)
parseMap message   =
    let
        key = take 4 message
        rest = drop 4 message
    in
        if key /= "last"
            then case parse' rest of
                    Right (value,r) -> Right (JLMap [(key,value)],r)
                    Left e -> Left e
                else case parseData rest of
                    Right(value,liekana) -> Right (JLMap [(key,value)],liekana)
                    Left e -> Left e

parseData :: String -> Either String (JsonLikeValue, String)
parseData ('d':'1':':':'0':'d':'4':':':rest) =
        let
            key = take 4 rest
            postfix = drop 4 rest
        in
            case parseDic postfix of
                Right (a,b) -> Right (JLMap [(key,Coordinates a)],consume (consume b))
                Left e -> Left e
parseData message = Left $ "error parseData " ++ message

parseDic :: String -> Either String ((Int,Int,Char),String)
parseDic ('d':'1':':':_:rest)=
        case parseInt rest of
            Left e -> Left e
            Right (a,n) -> case parseInt (consumeData n) of
                            Left e -> Left e
                            Right (c,m) -> case parseString (consumeData m) of
                                            Left e -> Left e
                                            Right (e,f) -> Right ((a,c,e),consume f)
        
parseDic _ = Left "error parseDIc"

parseString :: String -> Either String (Char,String)
parseString ('1':':':value:rest) = Right (value,rest)
parseString _ = Left "error parseString"

parseInt :: String -> Either String (Int,String)
parseInt ('i':remainder) =           
            let
                prefix = takeWhile isDigit remainder
                postfix = drop (length prefix) remainder
            in
                case postfix of
                    ('e':remnent) -> Right (read prefix,remnent)
                    _ -> Left "error parseInt"
parseInt _ = Left "error parseInt"

consumeData :: String -> String
consumeData ('1':':':_:rest) = rest
consumeData message = message

consume :: String -> String
consume ('e':rest) = rest
consume message = message
----------------------------------------------------------------------------------------------------------------------------------------

data InvalidState = Order | Duplicates | MessageMalformed  deriving (Show, Eq)

convert' :: JsonLikeValue ->Either String [(Int,Int,Char,Int)] 
convert' value = 
    case value of
        (JLMap (pirmas:r3)) -> case countMoves pirmas 0 of
                                        Left e-> Left e
                                        Right acc -> if r3 /= []
                                                        then
                                                            case goTillEnd r3 [] 0 of
                                                                Right acc2 -> Right $ acc++acc2
                                                                Left e-> Left e
                                                                    else Right acc
        _ -> Left "error in convert'"
goTillEnd :: [(String,JsonLikeValue)] ->[(Int,Int,Char,Int)]-> Int-> Either String [(Int,Int,Char,Int)]
goTillEnd (a:listas) acc position = case countMoves a position of
                                Left e-> Left e
                                Right values -> if listas /=[]
                                                then
                                                    goTillEnd listas (acc++values) position
                                                        else
                                                            Right (acc++values)
goTillEnd _ _ _= Left "error in goTillEnd"


countMoves ::   (String,JsonLikeValue) ->Int -> Either String [(Int,Int,Char,Int)]
countMoves value moves=
    case value of
         (key,jsonValue) -> 
                                    let
                                        numberOfMoves = moves+1
                                    in
                                        if key == "last"
                                            then
                                                convertLastJsonValue jsonValue numberOfMoves
                                                else
                                                    case jsonValue of
                                                        (JLMap a) -> goTillEnd a [] numberOfMoves
                                                        _ -> Left "error in countMoves"

convertLastJsonValue ::   JsonLikeValue -> Int->Either String [(Int,Int,Char,Int)]
convertLastJsonValue value numberOfMoves=
    case value of
        JLMap [(_,Coordinates (a,b,c))] -> Right [(a,b,c,numberOfMoves)]
        _-> Left "Cannot convert' last value"

convert :: String ->Either String [(Int,Int,Char,Int)]
convert message = case parse 3 message of
                    Right value -> convert' value
                    Left _ -> Left "malformed"


convertOrder :: String -> Either InvalidState [(Int,Int,Char)]
convertOrder message =  
    case convert message of
            Left _ -> Left MessageMalformed
            Right value -> 
                let
                   coords = onlyCoords (sortByMoves value) []
                in
                    (if isThereDuplicate (changeFormat coords []) then
                        Left Duplicates
                            else
                                if isOrderIncorrect (lineOfOrder coords [] )
                                    then
                                        Left Order
                                        else
                                            Right coords)
        

sortByMoves :: [(Int,Int,Char,Int)] -> [(Int,Int,Char,Int)]
sortByMoves  = sortBy (flip compare `on` frth) 

onlyCoords :: [(Int,Int,Char,Int)] -> [(Int,Int,Char)] -> [(Int,Int,Char)]
onlyCoords [] acc = reverse acc
onlyCoords ((x,y,v,_):rest) acc = onlyCoords rest ((x,y,v):acc)

frth :: (a,b,c,d) -> d
frth (_,_,_,d)  = d

isThereDuplicate :: [(Int,Int)] -> Bool
isThereDuplicate [] = False
isThereDuplicate [_] = False
isThereDuplicate (h:t) = elem h t || isThereDuplicate t

isOrderIncorrect :: String -> Bool
isOrderIncorrect [] = False
isOrderIncorrect [_] = False
isOrderIncorrect (h:t) = (h == head t) || isOrderIncorrect t

lineOfOrder :: [(Int,Int,Char)] -> String -> String
lineOfOrder [] acc = reverse acc
lineOfOrder ((_,_,c):rest) acc= lineOfOrder rest (c:acc)

changeFormat :: [(Int,Int,Char)] -> [(Int,Int)] -> [(Int,Int)]
changeFormat [] acc = reverse acc
changeFormat ((a,b,_):rest) acc = changeFormat rest ((a,b):acc)

-----------------------------------------------------------------------------------------------

initialBoard :: [(Int,Int,Char)]
initialBoard = [(a,b,'B') | a <- [0..2], b <- [0..2]]

replaceInitialBoard :: [(Int,Int,Char)] -> [(Int,Int,Player)]
replaceInitialBoard givenBoard = checkIfCellIsOccupied initialBoard givenBoard []

checkIfCellIsOccupied :: [(Int,Int,Char)] -> [(Int,Int,Char)] -> [(Int,Int,Player)] -> [(Int,Int,Player)]
checkIfCellIsOccupied [] _ acc = acc
checkIfCellIsOccupied ((a,b,_):board) gameBoard acc =   if elem (a, b, 'X') gameBoard then
                                                            checkIfCellIsOccupied board gameBoard acc ++ [(a, b, X)]
                                                        else
                                                            case elem (a, b, 'O') gameBoard of
                                                                True -> checkIfCellIsOccupied board gameBoard acc ++ [(a, b, O)]
                                                                False -> checkIfCellIsOccupied board gameBoard acc ++ [(a, b, B)]

second :: (a, b, c) -> b
second (_,b,_)  = b

first :: (a, b,c) -> a
first (a,_,_) = a

sortByY :: [(Int,Int,Player)] -> [(Int,Int,Player)]
sortByY  = sortBy (compare `on` second)

sortByX :: [(Int,Int,Player)] -> [(Int,Int,Player)]
sortByX = sortBy (compare `on` first)

grouping :: Eq a1 => [(a2, a1, c)] -> [[(a2, a1, c)]]
grouping list = groupBy ((==) `on` second) list

sortXY :: [[(Int, Int, Player)]] -> [[(Int, Int, Player)]]
sortXY list = map (sortByX) list

sortBoard :: [(Int,Int,Player)] -> [[(Int,Int,Player)]]
sortBoard list =sortXY groupedByY
    where
        sortedByY = sortByY list
        groupedByY = grouping sortedByY

changeForm :: [[(Int,Int,Player)]] -> Board
changeForm [] = []
changeForm ([(_,_,a),(_,_,b),(_,_,c)]:rest) = chop 3 (concat ([[a]] ++ [[b]] ++ [[c]] ++ (changeForm rest)))
