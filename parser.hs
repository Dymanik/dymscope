module Parser where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import qualified Text.ParserCombinators.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative
import Data.Functor
import qualified Control.Monad.State as St
import qualified Control.Monad.RWS as RWS
import Data.Map as Map
import Data.Maybe
import Control.Monad

type Ident = String
--type Block = [Stmt]
type FunctArgs = [(Ident,Type)]

newtype Block = Block [Stmt]
		deriving Show

data Stmt =   DeclVar Ident Type
			| DeclFunct Ident FunctArgs Block 
			| DeclProc Ident FunctArgs Block 
			| Assign Ident AExpr
			| CallProc Ident [AExpr]
			| ControlIf BExpr Block Block
			| Return (Maybe AExpr)
	deriving Show

type Type = String
{-
 -data Type =   TInt
 -            | TFunc
 -            | TProc
 -            | TBool
 -        deriving Show
 -}

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
					"var",
					"int"
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

stmts = Block <$> many stmt

stmt =  declFunct
	<|> declProc
	<|> funReturn
	<|> controlIf
	<|> (declVar
	<|> (identifier >>=( \x-> CallProc x <$> parens args <|> (Assign x <$> (reservedOp "=" *> aexpr)))) 
	) <* semi

funstmts = many (funReturn <|> stmt)

funReturn = Return <$> (reserved "return" *> optionMaybe aexpr <* semi)

--Variable Declaration
declVar =  DeclVar <$> (reserved "var" *> identifier) <*> (reservedOp ":" *> varType)

varType = (reserved "int" *> return "int")
		<|> (reserved "proc" *> return "proc")
		<|> (reserved "function" *> return "function")

	

--Function Declaration
declFunct = DeclFunct <$> (reserved "function" *> identifier) <*> parens functdeclargs <*> braces stmts

declProc = DeclProc <$> (reserved "proc" *> identifier) <*> parens functdeclargs <*> braces stmts

{-
 -functdeclargs = commaSep identifier
 -}

functdeclargs = commaSep (pair <$> identifier <*> (reservedOp ":" *> varType))
		where
			pair a b = (a,b)

assign = Assign <$> identifier <*> (reservedOp "=" *> aexpr)

callProc = CallProc <$> identifier <*> parens args 

args = commaSep aexpr


controlIf = ControlIf <$> (reserved "if" *> parens bexpr) <*> braces stmts <*> (reserved "else" *> braces stmts <|> Block <$> return [])

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


--------------evaluation (dynamic scope)--------

data SymbolTable = SymbolTable {
					symtableValues :: Map String (Type,SymbolValue),
					symtableEnv :: Maybe [SymbolTable],
					symtableName :: String,
					symtableLexicalParent :: String
				}
				deriving Show

data SymbolValue =	Uninitialized
				|	ValInt Integer
				|	ValFun FunctArgs Block (Maybe [SymbolTable])
				|	Void
				|	Exit
				deriving Show

data Environment = Environment {
					envStack :: [SymbolTable],
					envCallStack  :: [(String,[SymbolTable])],
					envScope :: Integer
				}			
				deriving Show

data ScopeConfig = ScopeConfig {
						deepBinding :: Bool,
						lookupScope :: String-> RWS.RWS ScopeConfig [String] Environment SymbolValue,
						assignScope	:: String->SymbolValue -> RWS.RWS ScopeConfig [String] Environment SymbolValue
				}


dynShallowScop = ScopeConfig False lookupDyn assignDyn
dynDeepScop = ScopeConfig True lookupDyn assignDyn
staticShallowScop = ScopeConfig False lookupDyn assignDyn
staticDeepScop = ScopeConfig True lookupDyn assignDyn

lookupDyn = undefined
assignDyn = undefined
lookupStatic = undefined
assignStatic = undefined

{-emptySymtable ::  String -> String -> SymbolTable-}
emptySymtable =  SymbolTable Map.empty 


insertSymbol name val = do{
						env<-St.get;
						let x = insert' name val (envStack env) in
						St.put $ env{envStack=x}
					}
			where
				insert' a b (x:xs) = x{symtableValues=insert a b (symtableValues x)} : xs

assignSymbol name val = RWS.asks assignScope >>= (\f -> f name val)


newEnvironment = Environment [emptySymtable Nothing "Global" ""] [("",[])] 0

evalProg ::  Stmt -> ScopeConfig -> (SymbolValue, Environment,[String])
evalProg block conf = RWS.runRWS (eval block) conf newEnvironment

eval :: Stmt -> RWS.RWS ScopeConfig [String]  Environment SymbolValue
eval (DeclVar name typ) = insertSymbol name (typ,Uninitialized) >> return Void
eval (DeclFunct name args block) = insertSymbol name ("funct", ValFun args block Nothing) >>  return Void
eval (DeclProc name args block) = insertSymbol name ("proc",ValFun args block Nothing ) >> return Void
eval (Assign name expr) = evalExpr expr >>= assignSymbol name >>return Void
eval (CallProc name args) = RWS.asks lookupScope >>=
									(\f-> f name) >>= 
										(\x -> case x of
											ValFun fargs block env -> insertArgs fargs <$> mapM evalExpr args >> evalBlock block
											otherwise -> error "Expected Procedure"
										)
						where
							insertArgs :: [(Ident,Type)] -> [SymbolValue] -> RWS.RWS ScopeConfig [String]  Environment [()]
							insertArgs = zipWithM (\(n,t) v-> insertSymbol n (t,v))
eval (ControlIf cond tblock fblock) = do
									x <- evalBool cond;
									if x then evalBlock tblock else evalBlock fblock
eval (Return a) =maybe (return Exit)  evalExpr a

{-evalBlock :: Block -> RWS.RWS Integer [String] Environment Integer-}
evalBlock (Block b) = eval' b
		where
			eval' [] = return Void
			eval' (x:xs) = eval x >>=(\ret -> case ret of
					 								Void -> eval' xs
													otherwise -> return ret)

evalBool (ValB x) = return x
evalBool (Or a b) = (||) <$> evalBool a <*> evalBool b
evalBool (And a b) = (&&) <$> evalBool a <*> evalBool b
evalBool (Not a) = not <$> evalBool a
evalBool (Comp r a b) = operBinValInt (compare r) <$> evalExpr a <*> evalExpr b
			where
				compare LessT = (<)
				compare LessTE = (<=)
				compare GreaterTE = (>=)
				compare GreaterT = (>)
				compare Equals = (==)
				compare NEquals = (/=)
	
evalExpr (Val n) = return $ ValInt n
evalExpr (Negate a) = operValInt negate <$> evalExpr a
evalExpr (Var n) =do{
					x <- lookupDynShallow n;
					case x of
						("int",x) -> return x
						(_,Uninitialized) -> error (n++" is undefined")
						otherwise -> error "wrongtype: expected int"
					}
evalExpr (CallFunct n args)  = undefined
evalExpr (BinAOp op a b) = ValInt <$> (operBinValInt ( oper op) <$> evalExpr a <*>  evalExpr b)
			where
				oper Add = (+)
				oper Mult = (*)
				oper Div = div
				oper Minus = (-)
				oper Mod = mod

lookupDynShallow name = (find name . envStack) <$> St.get
			where
				find name (x:xs) = fromMaybe (find name xs) (Map.lookup name (symtableValues x))


operValInt f (ValInt x) = ValInt (f x)

operBinValInt f (ValInt x) (ValInt y)= f x y

{-
 -assignDynShallow name val = (find name . envStack) <$> St.get
 -                where 
 -                    find name (x:xs) = maybe (x:find name xs) (\s-> update s val :xs) (Map.lookup name (symtableValues x))
 -                    update ("int",_)  = 
 -}
