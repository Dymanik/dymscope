module Parser where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import qualified Text.ParserCombinators.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative
import Data.Functor
import qualified Control.Monad.State as St
import Data.Map

type Ident = String
type Block = [Stmt]
type FunctArgs = [String]

data Stmt =   DeclVar Ident
			| DeclFunct Ident FunctArgs Block 
			| DeclProc Ident FunctArgs Block 
			| Assign Ident AExpr
			| CallProc Ident [AExpr]
			| ControlIf BExpr Block Block
			| Return AExpr
	deriving Show


data AExpr = Negate AExpr 
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
		reservedOpNames = ["+","-","/","*","=","&&","||","<",">","<=",">=","==","!==","!", ":"],
		reservedNames = [
					"function",
					"return",
					"proc",
					"if",
					"else",
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
	<|> funReturn
	<|> controlIf
	<|> (declVar
	<|> (identifier >>=( \x-> CallProc x <$> parens args <|> (Assign x <$> (reservedOp "=" *> aexpr)))) 
	) <* semi

funstmts = many (funReturn <|> stmt)

funReturn = Return <$> (reserved "return" *> aexpr <* semi)

--Variable Declaration
declVar =  DeclVar <$> (reserved "var" *> identifier) 

--Function Declaration
declFunct = DeclFunct <$> (reserved "function" *> identifier) <*> parens functdeclargs <*> braces stmts

declProc = DeclProc <$> (reserved "proc" *> identifier) <*> parens functdeclargs <*> braces stmts

functdeclargs = commaSep identifier

assign = Assign <$> identifier <*> (reservedOp "=" *> aexpr)

callProc = CallProc <$> identifier <*> parens args 

args = commaSep aexpr


controlIf = ControlIf <$> (reserved "if" *> parens bexpr) <*> braces stmts <*> (reserved "else" *> braces stmts <|> return [])

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


--------------evaluation---------


data SymbolVal = SymInt Integer 
			


eval :: St.State [Map String symbolVal] Int -> [stmt]
eval  s = undefined


{-
 -data BExpr = Or BExpr BExpr
 -            | And BExpr BExpr
 -            | Comp Rel AExpr AExpr
 -            | Not BExpr
 -            | ValB Bool
 -    deriving Show
 -}


evalBool ::  BExpr -> Bool
evalBool (ValB x) = x
evalBool (Or a b) = evalBool a || evalBool b
evalBool (And a b) = evalBool a && evalBool b
evalBool (Not a) = not $ evalBool a
evalBool (Comp r a b) = (compare r)(evalExpr a) ( evalExpr b)
			where
				compare LessT = (<)
				compare LessTE = (<=)
				compare GreaterTE = (>=)
				compare GreaterT = (>)
				compare Equals = (==)
				compare NEquals = (/=)
	
evalExpr :: AExpr -> Integer
evalExpr (Val n) = n
evalExpr (Negate a) = -(evalExpr a)
evalExpr (Var n) = undefined
evalExpr (CallFunct n args)  = undefined
evalExpr (BinAOp op a b) = (oper op) (evalExpr a) (evalExpr b)
			where
				oper Add = (+)
				oper Mult = (*)
				oper Div = div
				oper Minus = (-)
				oper Mod = mod
