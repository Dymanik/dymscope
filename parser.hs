module Parser where
import Prelude hiding (foldr)
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import qualified Text.ParserCombinators.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Data.Functor
import Data.Map as Map hiding (map,foldr)
import Data.Maybe
import Data.Foldable
import Control.Applicative
import qualified Control.Monad.State as St
import qualified Control.Monad.RWS as RWS
import Control.Monad
import Control.DeepSeq
import System.IO.Unsafe

import Debug.Trace

type Ident = String
type Type = String
--type Block = [Stmt]
type FunctArgs = [(Ident,Type)]

data Block = Block Integer [Stmt]
		deriving Show

data Stmt =   DeclVar Ident Type
			| DeclFunct Ident FunctArgs Block 
			| DeclProc Ident FunctArgs Block 
			| Assign Ident AExpr
			| CallProc Ident [AExpr]
			| ControlIf BExpr Block Block
			| CallPrint AExpr
			| CallPrintLn AExpr
			| Return (Maybe AExpr)
	deriving Show

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
					"int",
					"print",
					"println"
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

stmts = Block <$> (updateState (+1) >> getState ) <*> many stmt

stmt =  declFunct
	<|> declProc
	<|> funReturn
	<|> controlIf
	<|> (declVar
	<|> callPrintLn
	<|> callPrint
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

callPrint = CallPrint <$> (reserved "print" *> parens aexpr)
callPrintLn = CallPrintLn <$> (reserved "println" *> parens aexpr)
args = commaSep aexpr


controlIf = ControlIf <$> (reserved "if" *> parens bexpr) <*> braces stmts <*> (reserved "else" *> braces stmts <|> Block  <$> (getState) <*> return [])

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
					symtableEnv :: Maybe (Map String Integer),
					symtableStackPos :: Integer,
					symtableName :: String,
					symtableLexicalParent :: String
				}
				deriving Show

data SymbolValue =	Uninitialized
				|	ValInt Integer
				|	ValFun FunctArgs Block String (Maybe (Map String Integer))
				|	Void
				|	Exit SymbolValue
				deriving Show

{-
 -instance Show SymbolValue where
 -    show (ValInt x) = show x
 -    show (Uninitialized) = "Uninitialized"
 -    show (Void) = "Void"
 -    show (ValFun args _ _ _) = "(" ++ show args ++ ")"
 -}


data Environment = Environment {
					envStack :: [SymbolTable],
					envCallStack  :: [(String,[SymbolTable])],
					envScope :: Integer
				}			
				deriving Show

data ScopeConfig = ScopeConfig {
						deepBinding :: Bool,
						staticScoping :: Bool,
						lookupScope :: String-> RWS.RWS ScopeConfig [String] Environment (Maybe (Type,SymbolValue)),
						assignScope	:: String->(Type,SymbolValue) -> RWS.RWS ScopeConfig [String] Environment ()
				}


getBindings ::  RWS.RWS ScopeConfig [String]  Environment (Map String Integer)
getBindings =  RWS.asks staticScoping >>= \x-> if x
				then  foldr ((union . Map.fromList) . (\x-> map (\y->(y,symtableStackPos x)) (keys $symtableValues x))) Map.empty <$>RWS.gets (staticChain .envStack)
				else  foldr ((union . Map.fromList) . (\x-> map (\y->(y,symtableStackPos x)) (keys $symtableValues x))) Map.empty <$>RWS.gets envStack

dynShallowScope ::  ScopeConfig
dynShallowScope = ScopeConfig False False lookupDyn assignDyn

dynDeepScope ::  ScopeConfig
dynDeepScope = ScopeConfig True False lookupDyn assignDyn

staticShallowScope ::  ScopeConfig
staticShallowScope = ScopeConfig False True lookupStatic assignStatic

staticDeepScope ::  ScopeConfig
staticDeepScope = ScopeConfig True True lookupStatic assignStatic

checktype :: (Type,SymbolValue) -> (Type,SymbolValue) -> Bool
checktype (t1,_) (t2,_) = t1 == t2

staticChain :: [SymbolTable] -> [SymbolTable]
staticChain [] = []
staticChain (x:xs) = x : f xs
				where 
					f = staticChain . dropWhile (\n-> symtableName n /= symtableLexicalParent x)



{- dinamic scopes -}
lookupDyn name = RWS.asks deepBinding >>= (\x -> if x
					then findDeep <$> RWS.gets envStack 
					else find <$> RWS.gets envStack
				)
				where
					findDeep [] = Nothing 
					findDeep l@(x:xs) = {-trace ("LU: "++name++" "++show l) $-}Map.lookup name (symtableValues x) `mplus` maybe (findDeep xs) (aux l <=< Map.lookup name) (symtableEnv x)
					aux l n = Map.lookup name $ symtableValues $  head $  dropWhile ( (n/=).symtableStackPos) l

					find  = foldr (mplus . Map.lookup name .symtableValues) Nothing

assignDyn name value  = RWS.asks deepBinding >>= (\x -> if x
					then RWS.modify (\x -> x{envStack=assignDeep $ envStack x})
					else RWS.modify (\x -> x{envStack=assign $ envStack x})
				)
				where
					{-assignDeep n _ [] = error (n++" variable not in scope")-}
					{-assignDeep n v (x:xs) = case assign' n v (symtableValues x) of -}
									{-(Nothing,_) -> x{symtableEnv=(snd .assign' n v) <$>symtableEnv x} : assignDeep n v xs-}
									{-(Just _, m) -> x{symtableValues=m}:xs-}
					assignDeep [] = error (name++" variable not in scope")
					assignDeep l@(x:xs) = case assign' (symtableValues x) of
									(Nothing,_) -> maybe (x:assignDeep xs) (aux l .fromJust . Map.lookup name) (symtableEnv x)
									(Just _,m)	-> x{symtableValues = m} : xs
					aux l n = (\(x,y) -> x ++ assign y) $ break ((n==) . symtableStackPos) l
					assign [] = error (name++" variable not in scope")
					assign (x:xs) = case assign' (symtableValues x) of 
									(Nothing,_) -> x:assign xs
									(Just _,m) -> x{symtableValues=m}:xs
					assign' = insertLookupWithKey (\_ a old -> if checktype a old then a else error ("expected" ++ fst old ++ " got " ++ fst a)) name value


{- static scope -}
lookupStatic name = RWS.asks deepBinding >>= \x-> if x
						then findDeep  <$> RWS.gets envStack <*> RWS.gets (staticChain . envStack)
						else find  <$> RWS.gets (staticChain . envStack)
					where
						findDeep  stack [] =  Nothing
						findDeep  stack (x:xs) = Map.lookup name (symtableValues x) `mplus` maybe (findDeep stack xs) (aux stack <=< Map.lookup name) (symtableEnv x)
						aux s n = Map.lookup name $ symtableValues $ head $ dropWhile ((n/=) . symtableStackPos) s
						find  = foldr (mplus . Map.lookup name . symtableValues ) Nothing


assignStatic name value  = {-trace ("ASSIGN: "++ name ++" "++show value ) $-} RWS.asks deepBinding >>= \x -> if x
						then RWS.modify (\x -> x{envStack=assignDeep $ envStack x})
						else RWS.modify (\x -> x{envStack=assign $ envStack x})
					where
						assignDeep [] = error (name ++ "variable not in scope")
						assignDeep l@(x:xs) = case assign' (symtableValues x) of
									(Nothing , _) -> maybe (x: next assignDeep (symtableLexicalParent x) xs) (aux l . fromJust . Map.lookup name) (symtableEnv x)
									(Just _ , m)  -> x{symtableValues=m}:xs
						aux l n = (\(x,y) -> x ++ assign y) $ break ((n==) . symtableStackPos) l
						assign [] = error (name ++ "variable not in scope")
						assign (x:xs) = case assign' (symtableValues x) of
									(Nothing,_)	-> x : next (assign) (symtableLexicalParent x) xs
									(Just _,m)	-> x{symtableValues=m}:xs
						assign' = insertLookupWithKey (\_ a old -> if checktype a old then a else error ("expected" ++ fst old ++ " got " ++ fst a)) name value
						next f p  = (\(x,y) -> x ++ f y) . span (\n -> symtableName n /= p)




{-emptySymtable ::  String -> String -> SymbolTable-}
emptySymtable :: Maybe (Map String Integer) -> Integer -> String -> String -> SymbolTable
emptySymtable =  SymbolTable Map.empty 

newScope name parent env= RWS.gets envStack >>= return stackPos >>=(\n ->  RWS.modify (\x -> x{envStack= emptySymtable env n name  parent : envStack x,envScope=1+envScope x}) ) {->> trace'-}
			where
				stackPos = RWS.gets envScope 
				{-trace' =  RWS.gets envScope >>= (\x -> trace ("ENTER: " ++ show (x-1)) (return x)) >> (RWS.gets (envStack) >>= \x -> trace (show x) (return Void))-}

exitScope	= {-trace' >> -}RWS.modify (\x -> x{envStack = tail ( envStack x) ,envScope=(envScope x)-1})
		{-where-}
			{-trace' =  RWS.gets envScope >>= (\x -> trace ("EXIT: " ++ show (x-1)) (return x)) >> (RWS.gets (head.tail.envStack) >>= \x -> trace (show x) (return Void))-}

insertSymbol name val binds = do{
						env<-St.get;
						{-binding <- getBindings;-}
						{-trace ("ENV="++ show env ++"\nname "++name++" <- "++ show val ++" bind="++ show (binds)) $-}
						let x = insert' name (val') (envStack env) in
						St.put $ env{envStack=x}
					}
			where
				insert' a b (x:xs) = x{symtableValues=insert a b (symtableValues x)} : xs
				val'  = case val of
					(t,ValFun a b n _) -> (t,ValFun a b n (binds))
					otherwise -> val

assignSymbol name val = RWS.asks assignScope >>= (\f -> f name val)

getCurrentStack :: RWS.RWS ScopeConfig [String] Environment String
getCurrentStack = RWS.gets (symtableName . head . envStack)



giveType ::  SymbolValue -> (Type, SymbolValue)
giveType val@(ValInt _) = ("int",val)
giveType val@(ValFun{}) = ("function",val)
giveType Uninitialized = error "\n Variable is unitialized" 

newEnvironment = Environment [emptySymtable Nothing (-1)  "Global" ""] [("",[])] 0
{-newEnvironment = Environment [] [] 0-}

printVal x = trace (show x) (return Void)
		where
			trace x v =	let str = ( unsafePerformIO . putStr) x
						in deepseq str $  return str >> v

printValLn x = trace (show x) (return Void)
		where 
			trace x v =	let str = ( unsafePerformIO . putStrLn) x
						in deepseq str $  return str >> v


evalProg ::  ScopeConfig -> [Stmt] -> (SymbolValue, Environment,[String])
evalProg conf block = RWS.runRWS (evalBlock (Block 0 block) "Global") conf newEnvironment

eval :: Stmt -> RWS.RWS ScopeConfig [String]  Environment SymbolValue
{-eval (CallPrint expr) = liftM (unsafePerformIO . print) (evalExpr expr)  >> return Void-}
eval (CallPrint expr) = evalExpr expr >>= printVal
eval (CallPrintLn expr) = evalExpr expr >>= printValLn
eval (DeclVar name typ) = insertSymbol name (typ,Uninitialized) Nothing >> return Void
eval (DeclFunct name args block) = getCurrentStack >>= (\x -> insertSymbol name ("function", ValFun args block x Nothing) Nothing) >>  return Void
eval (DeclProc name args block) = getCurrentStack >>= (\x -> insertSymbol name ("function",ValFun args block x Nothing ) Nothing) >> return Void
eval (Assign name expr) = giveType <$> evalExpr expr >>= assignSymbol name >>return Void
eval (CallProc name args) =  RWS.asks lookupScope >>=
									(\f-> fromMaybe (error $"Error: name not found "++name) <$> f name) >>= 
										(\x -> case x of
											("function",ValFun fargs block@(Block n _) parent env) -> mapM evalExpr args >>= insertArgs n parent fargs env  >> evalBlock block name >>= (\ret -> exitScope >> return ret)
											otherwise -> error "Expected Procedure"
										)
						where
							{-insertArgs ::String -> [(Ident,Type)] -> [SymbolValue] -> RWS.RWS ScopeConfig [String]  Environment [()]-}
							insertArgs n parent fargs env args = do {
								binds <- getBindings;
								newScope (name++"|"++(show n)++"|args") parent env;
								zipWithM (\(n,t) v-> insertSymbol n (t,v) (Just binds)) fargs args 
								}
eval (ControlIf cond tblock fblock) = do
									x <- evalBool cond;
									if x then evalBlock tblock "ifTbranch" else evalBlock fblock "ifFbranch"
eval (Return a) =liftM Exit (maybe (return Void)  evalExpr a)

{-evalBlock :: Block -> RWS.RWS Integer [String] Environment Integer-}
evalBlock (Block n b) name =  {-(RWS.gets envStack >>= \x -> trace (show x) (return Void))>>-}getCurrentStack >>= flip (newScope (name++"|"++(show n))) Nothing >> eval' b >>= (\x-> exitScope >> return x)
		where
			eval' [] = return Void
			eval' (x:xs) = eval x >>=(\ret -> case ret of
					 								Void -> eval' xs
													Exit val -> return val
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
evalExpr (Var n) =  do{
					x <- RWS.asks lookupScope >>= (\f -> f n);
					{-trace ("EVAL: "++n ++ "JUST" ++ show x)$-}
					case fromJust x of
						(_,Uninitialized) -> error (n++" is uninitialized")
						(_,x) -> return x
					}
evalExpr (CallFunct n args)  = eval (CallProc n args)
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



{- debugging -}

{-
 -printEnv :: RWS.RWS ScopeConfig [String] Environment SymbolValue
 -printEnv = (\x -> trace (show x) Void) <$> RWS.get
 -}


{-exec file mode = liftM (evalProg mode <$>) $ parseFromFile prog file-}
exec file mode =liftM ((evalProg mode <$>) . runParser prog 0 file ) (readFile file)
