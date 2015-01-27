module Parser where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import qualified Text.ParserCombinators.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Data.Functor
import Data.Map as Map
import Data.Maybe
import Data.Foldable
import Control.Applicative
import qualified Control.Monad.State as St
import qualified Control.Monad.RWS as RWS
import Control.Monad
import Debug.Trace

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
					symtableEnv :: Maybe (Map String (Type,SymbolValue)),
					symtableName :: String,
					symtableLexicalParent :: String
				}
				deriving Show

data SymbolValue =	Uninitialized
				|	ValInt Integer
				|	ValFun FunctArgs Block String (Maybe (Map String (Type,SymbolValue)))
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
						lookupScope :: String-> RWS.RWS ScopeConfig [String] Environment (Maybe (Type,SymbolValue)),
						assignScope	:: String->(Type,SymbolValue) -> RWS.RWS ScopeConfig [String] Environment ()
				}


dynShallowScope ::  ScopeConfig
dynShallowScope = ScopeConfig False lookupDyn assignDyn

dynDeepScope ::  ScopeConfig
dynDeepScope = ScopeConfig True lookupDyn assignDyn

staticShallowScope ::  ScopeConfig
staticShallowScope = ScopeConfig False lookupDyn assignDyn

staticDeepScope ::  ScopeConfig
staticDeepScope = ScopeConfig True lookupDyn assignDyn

checktype :: (Type,SymbolValue) -> (Type,SymbolValue) -> Bool
checktype (t1,_) (t2,_) = t1 == t2

staticChain :: [SymbolTable] -> [SymbolTable]
staticChain [] = []
staticChain (x:xs) = x : f xs
				where 
					f xs 	= staticChain $ dropWhile (\n-> symtableName n /= symtableLexicalParent x) xs 


{- dinamic scopes -}
lookupDyn name = RWS.asks deepBinding >>= (\x -> if x
					then findDeep name <$> RWS.gets envStack 
					else find name <$> RWS.gets envStack
				)
				where
					findDeep _ [] = Nothing
					findDeep name (x:xs) = Map.lookup name (symtableValues x) `mplus` maybe	(findDeep name xs) (Map.lookup name) (symtableEnv x)
					find _ [] = Nothing 
					find name (x:xs) = Map.lookup name (symtableValues x) `mplus` find name xs

assignDyn name value  = RWS.asks deepBinding >>= (\x -> if x
					then RWS.modify (\x -> x{envStack=assignDeep name value $ envStack x})
					else RWS.modify (\x -> x{envStack=assign name value $envStack x})
				)
				where
					assignDeep n _ [] = error (n++" variable not in scope")
					assignDeep n v (x:xs) = case assign' n v (symtableValues x) of 
									(Nothing,_) -> x{symtableEnv=(snd .assign' n v) <$>symtableEnv x} : assignDeep n v xs
									(Just _, m) -> x{symtableValues=m}:xs
					assign n _ [] = error (n++" variable not in scope")
					assign n v (x:xs) = case assign' n v (symtableValues x) of 
									(Nothing,_) -> x:assign n v xs
									(Just _,m) -> x{symtableValues=m}:xs
					assign' = insertLookupWithKey (\_ a old -> if checktype a old then a else error ("expected" ++ fst old ++ " got " ++ fst a))


{- static scope -}
lookupStatic name = RWS.asks deepBinding >>= \x-> if x
						then findDeep name <$> RWS.gets envStack
						else find name <$> RWS.gets envStack
					where
						findDeep _  [] = Nothing

						findDeep name (x:xs) = Map.lookup name (symtableValues x) `mplus` maybe (findDeep name (dropWhile (\n-> symtableName n /= symtableLexicalParent x) xs)) (Map.lookup name) (symtableEnv x) 

						find _ [] =Nothing
						find name (x:xs) = Map.lookup name (symtableValues x) `mplus` find name (dropWhile	(\n-> symtableName n /= symtableLexicalParent x) xs)

assignStatic name value  = RWS.asks deepBinding >>= \x -> if x
						then RWS.modify (\x -> x{envStack=assignDeep name value $ envStack x})
						else RWS.modify (\x -> x{envStack=assign name value $ envStack x})
					where
						assignDeep n _ [] =[]
						assignDeep n v (x:xs) = case assign' n v (symtableValues x) of
									(Nothing,_)	-> x{symtableEnv=(snd.assign' n v) <$> symtableEnv x}: next (assignDeep n v) (symtableLexicalParent x) xs
									(Just _,m)	-> x{symtableValues=m}:xs
						assign n _ [] = []
						assign n v (x:xs) = case assign' n v (symtableValues x) of
									(Nothing,_)	-> x : next (assign n v) (symtableLexicalParent x) xs
									(Just _,m)	-> x{symtableValues=m}:xs
						assign' = insertLookupWithKey (\_ a old -> if checktype a old then a else error ("expected" ++ fst old ++ " got " ++ fst a))
						next f p  = f' . span (\n -> symtableName n /= p)
								where
									f' (x,y) = x++ f y


{-emptySymtable ::  String -> String -> SymbolTable-}
emptySymtable :: Maybe (Map String (Type,SymbolValue)) -> String -> String -> SymbolTable
emptySymtable =  SymbolTable Map.empty 

newScope name parent env=newName >>=(\n ->  RWS.modify (\x -> x{envStack= emptySymtable env n parent : envStack x,envScope=1+envScope x}) )
			where
				newName = RWS.gets (((name++"|")++).show.envScope)

insertSymbol name val = do{
						env<-St.get;
						let x = insert' name val (envStack env) in
						St.put $ env{envStack=x}
					}
			where
				insert' a b (x:xs) = x{symtableValues=insert a b (symtableValues x)} : xs

assignSymbol name val = RWS.asks assignScope >>= (\f -> f name val)

getCurrentStack :: RWS.RWS ScopeConfig [String] Environment String
getCurrentStack = RWS.gets (symtableName . head . envStack)



giveType ::  SymbolValue -> (Type, SymbolValue)
giveType val@(ValInt _) = ("int",val)
giveType val@(ValFun{}) = ("function",val)
giveType Uninitialized = error "\n Variable is unitialized" 

newEnvironment = Environment [emptySymtable Nothing "Global" ""] [("",[])] 0

evalProg ::  ScopeConfig -> [Stmt] -> (SymbolValue, Environment,[String])
evalProg conf block = RWS.runRWS (evalBlock (Block block) "Global") conf newEnvironment

eval :: Stmt -> RWS.RWS ScopeConfig [String]  Environment SymbolValue
eval (DeclVar name typ) = insertSymbol name (typ,Uninitialized) >> return Void
eval (DeclFunct name args block) = getCurrentStack >>= (\x -> insertSymbol name ("function", ValFun args block x Nothing)) >>  return Void
eval (DeclProc name args block) = getCurrentStack >>= (\x -> insertSymbol name ("function",ValFun args block x Nothing )) >> return Void
eval (Assign name expr) = giveType <$> evalExpr expr >>= assignSymbol name >>return Void
eval (CallProc name args) = RWS.asks lookupScope >>=
									(\f-> fromMaybe (error "Error: name not found") <$> f name) >>= 
										(\x -> case x of
											("function",ValFun fargs block parent env) -> mapM evalExpr args >>= insertArgs parent fargs  >> evalBlock block name >> return Void
											otherwise -> error "Expected Procedure"
										)
						where
							insertArgs ::String -> [(Ident,Type)] -> [SymbolValue] -> RWS.RWS ScopeConfig [String]  Environment [()]
							insertArgs parent fargs args =  newScope (name++"|args") parent Nothing >>zipWithM (\(n,t) v-> insertSymbol n (t,v)) fargs args 
eval (ControlIf cond tblock fblock) = do
									x <- evalBool cond;
									if x then evalBlock tblock "ifTbranch" else evalBlock fblock "ifFbranch"
eval (Return a) =maybe (return Exit)  evalExpr a

{-evalBlock :: Block -> RWS.RWS Integer [String] Environment Integer-}
evalBlock (Block b) name = getCurrentStack >>= flip (newScope name) Nothing >> eval' b
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
					x <- RWS.asks lookupScope >>= (\f -> f n);
					case fromJust x of
						(_,Uninitialized) -> error (n++" is undefined")
						(_,x) -> return x
					}
evalExpr (CallFunct n args)  = undefined
evalExpr (BinAOp op a b) = ValInt <$> (operBinValInt ( oper op) <$> evalExpr a <*>  evalExpr b)
			where
				oper Add = (+)
				oper Mult = (*)
				oper Div = div
				oper Minus = (-)
				oper Mod = mod


operValInt ::  (Integer -> Integer) -> SymbolValue -> SymbolValue
operValInt f (ValInt x) = ValInt (f x)

operBinValInt :: (Integer -> Integer -> t) -> SymbolValue -> SymbolValue -> t
operBinValInt f (ValInt x) (ValInt y)= f x y

{-
 -assignDynShallow name val = (find name . envStack) <$> St.get
 -                where 
 -                    find name (x:xs) = maybe (x:find name xs) (\s-> update s val :xs) (Map.lookup name (symtableValues x))
 -                    update ("int",_)  = 
 -}
