{-# LANGUAGE InstanceSigs #-}

module Parser (parseASM, test_asmP) where

{-
Parse 3364 assembly files using Applicative Functors

Defines the classic Haskell Parser as String -> Maybe (a, String)
along with a small library of generic parsing functions followed by parsers
for various components of the ASM file.

ex: opP, labelP, operandP for parsing opcodes, labels and operands respectively

"higher order parsers" use <*> to compose parsers

-}


import Control.Applicative ( Alternative(..), optional )
import Data.Char ( isDigit, isSpace, isAlpha, toLower, isAlphaNum )
import Loader (loader)
import MachTypes
import Data.Monoid(First(..))
import Test.HUnit ( (~?=), Test(TestList), Counts, runTestTT )

-- TODO: add error messages during parsing
newtype Parser a = P {doParse :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f p = P $ \s -> 
                doParse p s >>=
                    \(a, s') -> return (f a, s')

instance Applicative Parser where
    pure :: a -> Parser a
    pure a = P $ \s -> Just (a, s)

    (<*>) :: Parser (t -> a) -> Parser t -> Parser a
    f <*> t = P $ \s -> 
                doParse f s >>=
                    \(f, s') -> 
                        doParse t s' >>=
                            \(t, s'') -> return (f t, s'')

instance Alternative Parser where
    empty :: Parser a
    empty = P $ const Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    p1 <|> p2 = P $ \s -> getFirst $ First (doParse p1 s) <> First (doParse p2 s)

get :: Parser Char
get = P $ \s ->
        case s of
            ""      -> Nothing
            (c:cs)  -> Just (c, cs)

filterP :: (a -> Bool) -> Parser a -> Parser a
filterP f p = P $ \s -> doParse p s >>= check f
    where
        check :: (a -> Bool) -> (a, String) -> Maybe (a, String)
        check guard (a, s')
            | guard a = Just (a, s')
            | otherwise = Nothing

takeWhileP :: (a -> Bool) -> Parser a -> Parser [a]
takeWhileP f p = (:) <$> filterP f p <*> (takeWhileP f p <|> pure [])

alphaChar :: Parser Char
alphaChar = filterP isAlpha get

digitChar :: Parser Char
digitChar = filterP isDigit get

-- valid characters for assembly labels
identifierChar :: Parser Char
identifierChar = alphaChar <|> digitChar <|> char '_'

char :: Char -> Parser Char
char = charCase . toLower

charCase :: Char -> Parser Char
charCase = flip filterP get . (==)

stringP :: String -> Parser String
stringP = foldr ((\c p -> (:) <$> char c <*> p) . toLower) (pure "")

eof :: Parser ()
eof = P $ \s -> 
        case s of
            "" -> Just ((), s)
            _ ->  Nothing

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = ((:) <$> p <*> many (sep *> p)) <|> pure []

numP :: Parser Integer
numP = minusP <*> fmap Prelude.read (some digitChar)
    where
        minusP :: Parser (Integer -> Integer)
        minusP = char '-' *> pure negate <|> pure id

hexP :: Parser String
hexP = stringP "0x" *> many (filterP (flip elem $ ['0'..'9'] ++ ['a'..'f']) get)

wsP :: Parser a -> Parser a
wsP p = p <* many (filterP isSpace get)

-- comments start with '#' and stop at the next line
commentP :: Parser ()
commentP = wsP $ char '#' *> optional (takeWhileP (/='\n') get) *> pure ()

-- labels are defined as one letter + any number of identifierChars
labelP :: Parser Label
labelP = (:) <$> filterP isAlpha get <*> many identifierChar

opP :: Parser Opcode
opP =   
        stringP "load" *> pure Load
    <|> stringP "store" *> pure Store
    <|> stringP "jumpnz" *> pure JumpNZ
    <|> stringP "jumpn" *> pure JumpN
    <|> stringP "jumpz" *> pure JumpZ
    <|> stringP "jump" *> pure Jump
    <|> stringP "add" *> pure Add
    <|> stringP "sub" *> pure Sub
    <|> stringP "mul" *> pure Mul
    <|> stringP "out" *> pure Out

maybeP :: Parser (Maybe a) -> Parser a
maybeP p = P $ \s -> doParse p s >>=
                     \(a, s') -> a >>=
                        \a -> return (a, s')

-- parsing operands (labels, hex or numeric values)
operandP :: Parser Operand
operandP = B . fromIntegral <$> numP <|> L <$> labelP 

-- parses ASM directives throwing out comments (lines starting with '#')
asmP :: Parser ASM
asmP = many commentP *> (wsP insnP <|> wsP defP)
    where
        insnP = Insn <$> wsP (optional (labelP <* char ':')) <*> wsP opP <*> wsP operandP
        defP = Def <$> wsP (labelP <* char ':') <* wsP (stringP ".data") <*> wsP numP

-- lift loading into a parser
programP :: Parser Program
programP = maybeP (loader <$> many asmP)

parse :: Parser a -> String -> Maybe a
parse p s = fst <$> doParse p s

-- run the program parser on some string input
parseASM :: String -> Maybe Program
parseASM = parse programP


test_asmP :: IO Counts
test_asmP = do
    runTestTT $
        TestList $ validAsmTests <> invalidAsmTests
    where
        validAsmTests =
            [ parse asmP "start:  load    istart" ~?= pure (Insn (Just "start") Load (L "istart"))
            , parse asmP "zero:   .data   0       #  store the constant '0'" ~?= pure (Def "zero" 0)
            , parse asmP "h1000:  .data   4096" ~?= pure (Def "h1000" 4096)
            , parse asmP "Kdelay: .data   768" ~?= pure (Def "Kdelay" 768)
            , parse asmP "add     outnum" ~?= pure (Insn Nothing Add (L "outnum"))
            , parse asmP "add 1" ~?= pure (Insn Nothing Add (B 1))
            , parse asmP "store   j" ~?= pure (Insn Nothing Store (L "j"))
            ]
        
        invalidAsmTests =
            [ parse asmP "h1000 4096" ~?= Nothing
            , parse asmP "# # abc #" ~?= Nothing
            , parse asmP "100: .data 1" ~?= Nothing
            , parse asmP "load !!!!" ~?= Nothing
            ]







