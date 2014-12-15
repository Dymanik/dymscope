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
			| Assign Ident Expr
			| CallFunctStmt Expr
			| Return Expr
	deriving Show


data Expr =   Add Expr Expr 
			| Mult Expr Expr 
			| Div Expr Expr
			| Minus Expr Expr
			| Negate Expr
			| Var Ident
			| Val Integer 
			| CallFunct Ident [Expr]
	deriving Show

-- Tokens

dymanikStyle ::  LanguageDef st
dymanikStyle = emptyDef { 
		commentStart = "/*",
		commentEnd 	= "*/",
		commentLine = "//",
		identStart = letter,
		identLetter = alphaNum <|> char '_',
		reservedOpNames = ["+","-","/","*","="],
		reservedNames = [
					"function",
					"return",
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

-- Parser
--
prog = whiteSpace *> stmts

stmts = many stmt  

stmt =  declFunct
	<|> (declVar
	<|> try assign
	<|> (CallFunctStmt <$> callFunct)
	) <* semi

funstmts = many (funReturn <|> stmt)

funReturn = Return <$> (reserved "return" *> expr <* semi)


--Variable Declaration
declVar =  DeclVar <$> (reserved "var" *> identifier) 

--Function Declaration
declFunct = DeclFunct <$> (reserved "function" *> identifier) <*> parens functdeclargs <*> braces funstmts

functdeclargs = commaSep (identifier)

assign = Assign <$> identifier <*> (reservedOp "=" *> expr)

callFunct = CallFunct <$> identifier <*> parens args 

args = commaSep expr



--Expresion Parsers


opTable = [ [ prefix "-" Negate],
			[ binary "*" Mult AssocLeft, binary "/" Div AssocLeft],
			[ binary "+" Add AssocLeft, binary "-" Minus AssocLeft]
		  ]

prefix op f  = Prefix ( reservedOp op *> return f) 

binary op f assoc = Infix (reservedOp op *> return f) assoc

expr =  buildExpressionParser opTable term

term =  parens expr
	<|> Val <$> integer
	<|> try callFunct
	<|> Var <$> identifier
