import Data.Char (isAlphaNum)
import Control.Applicative

data Token = 
    Word String
    | TrueKeyWord
    | FalseKeyWord
    | Assign
    | Dot
    | Comma
    | Open
    | Close
    | Question
    deriving (Show, Eq)

data Node = 
    Fact String [String]
    | Rule String [String] [Node]
    | Eval Node 
    deriving Show

continue :: Token -> String -> Maybe [Token]
continue (Word "") _ = Nothing
continue token rest = lexString rest >>= (\x -> Just (token : x))

lexString :: String -> Maybe [Token]
lexString "" = Just []
lexString ('t':'r':'u':'e':rest) = continue TrueKeyWord rest 
lexString ('f':'a':'l':'s':'e':rest) = continue FalseKeyWord rest 
lexString (':':'-':rest) = continue Assign rest
lexString ('.':rest) = continue Dot rest 
lexString (',':rest) = continue Comma rest
lexString ('(':rest) = continue Open rest
lexString (')':rest) = continue Close rest
lexString ('?':'-':rest) = continue Question rest
lexString (' ':rest) = lexString rest
lexString ('\n':rest) = lexString rest
lexString x = let (name, rest) = span isAlphaNum x 
              in continue (Word name) rest

parseFact :: [Token] -> Maybe (Node, [Token])
parseFact ((Word name):Open:r) = 
    let (content, rest) = span (/= Close) r
    in parseFactContent content >>= (\x -> Just (Fact name x, tail rest))
parseFact _ = Nothing

parseRule :: (Node, [Token]) -> Maybe (Node, [Token])
parseRule (Fact x y, Assign:rest) = 
    let (content, r) = span (/= Dot) rest
    in parseFact content >>= parseRuleContent >>= (\z -> Just (Rule x y z, tail r))
parseRule (Fact x y, Dot:rest) = Just (Fact x y, rest)
parseRule _ = Nothing

parseRuleContent :: (Node, [Token]) -> Maybe [Node]
parseRuleContent (Fact x y, Comma:rest) = parseFact rest >>= parseRuleContent >>= (\z -> Just (Fact x y : z))
parseRuleContent (Fact x y, []) = Just [Fact x y]
parseRuleContent _ = Nothing

parseFactContent :: [Token] -> Maybe [String]
parseFactContent [Word x] = Just [x]
parseFactContent ((Word x):Comma:rest) = parseFactContent rest >>= (\y -> Just (x : y))
parseFactContent _ = Nothing

parseSingle :: [Token] -> Maybe (Node, [Token])
parseSingle x = 
    let fact = parseFact x
        rule = fact >>= parseRule
    in rule <|> fact

parse :: [Token] -> Maybe [Node]
parse [] = Just []
parse x = parseSingle x >>= (\(node, tokens) -> parse tokens >>= (\y -> Just (node : y)))

-- TODO Parsing dots in parseFact

main = print (lexString "direct(london,prague) :- direct(penis, pero), direct(curak, kokot)." >>= parse)
