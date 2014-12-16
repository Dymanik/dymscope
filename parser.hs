module Main where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import qualified Text.ParserCombinators.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative
import Data.Functor

type Ident = String
type Block = [Stmt]
type FunctArgs = [String]

data Stmt =   DeclVar Ident
			| DeclFunct Ident FunctArgs Block 
			| DeclProc Ident FunctArgs Block 
			| Assign Ident AExpr
			| CallProc Ident [AExpr]
			| Return AExpr
	deriving Show


data AExpr = Negate AExpr 
			{-| Add AExpr AExpr-}
			{-| Mult AExpr AExpr -}
			{-| Div AExpr AExpr-}
			{-| Minus AExpr AExpr-}
			| BinAOp BinAOp AExpr AExpr
			| Var Ident
			| Val Integer 
			| CallFunct Ident [AExpr]
	deriving Show

data BinAOp = Add 
			| Mult
			| Div
			| Minus
			| Mod
	deriving Show

{-
 -data BinBOp = And
 -            | Or
 -}


data BExpr = Or BExpr BExpr
			| And BExpr BExpr
			| Comp Rel AExpr AExpr
			| Not BExpr
			| ValB Bool
	deriving Show

data Rel = LessT
		|  LessTE
		|  GreaterTE
		|  GreaterT
		|  Equals
		|  NEquals
	deriving Show

-- Tokens

dymanikStyle ::  LanguageDef st
dymanikStyle = emptyDef { 
		commentStart = "/*",
		commentEnd 	= "*/",
		commentLine = "//",
		identStart = letter,
		identLetter = alphaNum <|> char '_',
		reservedOpNames = ["+","-","/","*","=","&&","||","<",">","<=",">=","==","!==","!"],
		reservedNames = [
					"function",
					"return",
					"proc",
					"var"
					]
		}

lexer = Tok.makeTokenParser dymanikStyle
comma = Tok.comma lexer
semi = Tok.semi lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
identifier = Tok.identifier lexer
integer = Tok.integer lexer
braces = Tok.braces lexer 
parens = Tok.parens lexer 
commaSep = Tok.commaSep lexer 
whiteSpace = Tok.whiteSpace lexer 

-- Parser Grammar
--
prog = whiteSpace *> manyTill stmt eof

stmts = many stmt

stmt =  declFunct
	<|> declProc
	<|> (declVar
	<|> (identifier >>=( \x-> CallProc x <$> parens args <|> (Assign x <$> (reservedOp "=" *> aexpr)))) 
	) <* semi

funstmts = many (funReturn <|> stmt)

funReturn = Return <$> (reserved "return" *> aexpr <* semi)

--Variable Declaration
declVar =  DeclVar <$> (reserved "var" *> identifier) 

--Function Declaration
declFunct = DeclFunct <$> (reserved "function" *> identifier) <*> parens functdeclargs <*> braces funstmts

declProc = DeclProc <$> (reserved "proc" *> identifier) <*> parens functdeclargs <*> braces stmts

functdeclargs = commaSep identifier

assign = Assign <$> identifier <*> (reservedOp "=" *> aexpr)

callProc = CallProc <$> identifier <*> parens args 

args = commaSep aexpr

--Expresion Parsers


opTable = [ [ prefix "-" Negate],
			[ binary "*" (BinAOp Mult) AssocLeft, binary "/" (BinAOp Div) AssocLeft],
			[ binary "+" (BinAOp Add) AssocLeft, binary "-" (BinAOp Minus) AssocLeft]
		  ]

prefix op f  = Prefix ( reservedOp op *> return f) 

binary op f  = Infix (reservedOp op *> return f) 


aexpr =  buildExpressionParser opTable term

term =  parens aexpr
	<|> Val <$> integer
	<|> (identifier >>=( \x-> CallFunct x <$> parens args <|> return ( Var x))) 

opTableB = [ [ prefix "!" Not],
			[ binary "&&" And AssocLeft, binary "||" Or AssocLeft]
		  ]

bexpr = buildExpressionParser opTableB bterm


bterm = parens bexpr
	<|> (reserved "true" >> return (ValB True))
	<|> (reserved "false" >> return (ValB False))
	<|> rterm


rterm = flip Comp <$> aexpr <*> relation <*> aexpr

relation  = reservedOp "<" *> return LessT
		<|> reservedOp "<=" *> return LessTE
		<|> reservedOp "==" *> return Equals
		<|> reservedOp "!==" *> return NEquals
		<|> reservedOp ">=" *> return GreaterTE
		<|> reservedOp ">" *> return GreaterT

