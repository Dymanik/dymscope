module Parser (
			exec,
			dynDeepScope,
			dynShallowScope,
			staticShallowScope,
			staticDeepScope,
			ScopeConfig,
			) where
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


type Ident = String
{-type Type = String-}
--type Block = [Stmt]
type FunctArgs = [(Ident,Type)]

data Block = Block Integer [Stmt]
		deriving Show
	
instance Eq Block where
	(Block a _) == (Block b _) = a == b

data Stmt =   DeclVar Ident Type
			| DeclFunct Ident FunctArgs Type Block 
			| Assign Ident Expr
			| CallProc Ident [Expr]
			| ControlIf Expr Block Block
			| CallPrint Expr
			| CallPrintLn Expr
			| Return (Maybe Expr)
	deriving Show

data Type =   TInt
			| TFunct
			| TBool
			| TString
			| TVoid
			deriving (Eq)

instance Show Type where
	show (TInt) = "int"
	show (TFunct) = "sub"
	show (TBool) = "bool"
	show (TString) = "String"
	show (TVoid) = "Void"

data Expr =   Var Ident
			| IntLiteral Integer
			| BoolLiteral Bool
			| StringLiteral String
			| CallFunct Ident [Expr]
			| UnaryExp UnaryOP Expr
			| BinaryExp BinaryOP Expr Expr
			deriving Show

data UnaryOP = Negate
			|  Not
			deriving Show

data BinaryOP =   Add {-Aritmetic-}
				| Mult
				| Div
				| Minus
				| Mod
				| And {-Boolean-}
				| Or
				| LessT {-Comparisons-}
				| LessTE
				| GreaterT
				| GreaterTE
				| Equals
				| NEquals
			deriving Show


-- Tokens

dymanikStyle ::  LanguageDef st
dymanikStyle = emptyDef { 
		commentStart = "/*",
		commentEnd 	= "*/",
		commentLine = "//",
		identStart = letter,
		identLetter = alphaNum <|> char '_',
		reservedOpNames = ["+","-","/","*","=","&&","||","<",">","<=",">=","==","!=","!", ":"],
		reservedNames = [
					"sub",
					"return",
					"proc",
					"if",
					"else",
					"var",
					"int",
					"bool",
					"Void",
					"print",
					"printLn"
					]
		}

lexer = Tok.makeTokenParser dymanikStyle
comma = Tok.comma lexer
semi = Tok.semi lexer
reserved = Tok.reserved lexer
reservedOp = Tok.reservedOp lexer
identifier = Tok.identifier lexer
integer = Tok.integer lexer
stringLiteral = Tok.stringLiteral lexer 
braces = Tok.braces lexer 
parens = Tok.parens lexer 
commaSep = Tok.commaSep lexer 
whiteSpace = Tok.whiteSpace lexer 



-- Parser Grammar
--
prog = whiteSpace *> manyTill stmt eof

stmts = Block <$> (updateState (+1) >> getState ) <*> many stmt

stmt =  declFunct
	<|> funReturn
	<|> controlIf
	<|> (declVar
	<|> callPrintLn
	<|> callPrint
	<|> (identifier >>=( \x-> CallProc x <$> parens args <|> (Assign x <$> (reservedOp "=" *> expr) <?> "Subroutine Call o Assignment"))) 
	) <* semi

funReturn = Return <$> (reserved "return" *> optionMaybe expr <* semi) <?> "return"

--Variable Declaration
declVar =  DeclVar <$> (reserved "var" *> identifier) <*> (reservedOp ":" *> varType) <?> "variable declaration"

varType = (reserved "int" *> return TInt)
		<|> (reserved "sub" *> return TFunct)
		<|> (reserved "bool" *> return TBool)
		<?> "type"

funcType = varType <|> (reserved "Void" *> return TVoid)	

--Function Declaration
declFunct = DeclFunct <$> (reserved "sub" *> identifier) <*> parens functdeclargs <*> (reservedOp ":" *>funcType) <*> braces stmts <?> "function declaration"


{-
 -functdeclargs = commaSep identifier
 -}

functdeclargs = commaSep (pair <$> identifier <*> (reservedOp ":" *> varType))
		where
			pair a b = (a,b)

{-assign = Assign <$> identifier <*> (reservedOp "=" *> expr)-}

{-callProc = CallProc <$> identifier <*> parens args -}

callPrint = CallPrint <$> (reserved "print" *> parens expr)
callPrintLn = CallPrintLn <$> (reserved "printLn" *> parens expr)
args = commaSep expr


controlIf = ControlIf <$> (reserved "if" *> parens expr) <*> braces stmts <*> (reserved "else" *> braces stmts <|> Block  <$> getState <*> return [])

--Expresion Parsers


opTable = [ [ prefix "-" (UnaryExp Negate), prefix "!" (UnaryExp Not)],
			[ binary "*" (BinaryExp Mult) AssocLeft, binary "/" (BinaryExp Div) AssocLeft , binary "%" (BinaryExp Mod) AssocLeft ],
			[ binary "+" (BinaryExp Add) AssocLeft, binary "-" (BinaryExp Minus) AssocLeft],
			[ binary "<" (BinaryExp LessT) AssocLeft, binary "<=" (BinaryExp LessTE) AssocLeft , binary ">" (BinaryExp GreaterT) AssocLeft , binary ">=" (BinaryExp GreaterTE) AssocLeft],
			[ binary "==" (BinaryExp Equals) AssocLeft, binary "!=" (BinaryExp NEquals) AssocLeft],
			[ binary "&&" (BinaryExp And) AssocLeft],
			[ binary "||" (BinaryExp Or) AssocLeft]
		  ]
		  where
		  	prefix op f  = Prefix ( reservedOp op *> return f) 
			binary op f  = Infix (reservedOp op *> return f) 


expr =  buildExpressionParser opTable term

term =  parens expr
	<|> IntLiteral <$> integer
	<|> (reserved "true" >> return  (BoolLiteral True))
	<|> (reserved "false" >> return (BoolLiteral False))
	<|> StringLiteral <$> stringLiteral
	<|> (identifier >>=( \x-> CallFunct x <$> parens args <|> return ( Var x))) 
	<?> "Expression"

--------------evaluation --------

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
				|	ValBool Bool 
				|	ValString String
				|	ValFun FunctArgs Type Block String (Maybe (Map String Integer))
				|	Void
				|	Exit SymbolValue
				deriving (Show,Eq)

instance NFData SymbolValue where
	rnf a = a `seq` ()

data Environment = Environment {
					envStack :: [SymbolTable],
					{-envCallStack  :: [(String,[SymbolTable])],-}
					envScope :: Integer
				}			
				deriving Show

instance NFData Environment where
	rnf a = a `seq`  ()

data ScopeConfig = ScopeConfig {
						deepBinding :: Bool,
						staticScoping :: Bool,
						lookupScope :: String-> RWS.RWST ScopeConfig Log Environment (Either EvalError) (Maybe (Type,SymbolValue)),
						assignScope	:: String->(Type,SymbolValue) -> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
				}


type Log = [String]


{-- EXCEPTION --}
data EvalError =  NotInScope Ident
				| VariableNotInitialized Ident
				| WrongType Type Type
				| LessArgs Ident Int Int

instance Show EvalError where
	show (NotInScope n) = "identifier "++show n++" not in scope"
	show (VariableNotInitialized n) = "variable "++show n++" has not been initialized"
	show (WrongType a b) = "unexpected "++show a++" expected "++show b
	show (LessArgs n a b) = "called sub "++show n++" with "++show a++" arguments expected "++ show b




getBindings =  RWS.asks staticScoping >>= \x-> if x
				then  foldr ((union . Map.fromList) . (\y-> map (\z->(z,symtableStackPos y)) (keys $symtableValues y))) Map.empty <$>RWS.gets (staticChain .envStack)
				else  foldr ((union . Map.fromList) . (\y-> map (\z->(z,symtableStackPos y)) (keys $symtableValues y))) Map.empty <$>RWS.gets envStack

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
					else find' <$> RWS.gets envStack
				)
				where
					findDeep [] = Nothing 
					findDeep l@(x:xs) = Map.lookup name (symtableValues x) `mplus` maybe (findDeep xs) (aux l <=< Map.lookup name) (symtableEnv x)
					aux l n = Map.lookup name $ symtableValues $  head $  dropWhile ( (n/=).symtableStackPos) l

					find'  = foldr (mplus . Map.lookup name .symtableValues) Nothing

assignDyn name value  = RWS.asks deepBinding >>= (\x -> if x
					then RWS.modify (\y -> y{envStack=assignDeep $ envStack y})
					else RWS.modify (\y -> y{envStack=assign $ envStack y})
				)
				where
					assignDeep [] = error (name++" variable not in scope")
					assignDeep l@(x:xs) = case assign' (symtableValues x) of
									(Nothing,_) -> maybe (x:assignDeep xs) (aux l .fromJust . Map.lookup name) (symtableEnv x)
									(Just _,m)	-> x{symtableValues = m} : xs
					aux l n = (\(x,y) -> x ++ assign y) $ break ((n==) . symtableStackPos) l
					assign [] = error (name++" variable not in scope")
					assign (x:xs) = case assign' (symtableValues x) of 
									(Nothing,_) -> x:assign xs
									(Just _,m) -> x{symtableValues=m}:xs
					assign' = insertLookupWithKey (\_ a old -> if checktype a old then a else error ("expected" ++ show (fst old) ++ " got " ++ show (fst a))) name value


{- static scope -}
lookupStatic name = RWS.asks deepBinding >>= \x-> if x
						then findDeep  <$> RWS.gets envStack <*> RWS.gets (staticChain . envStack)
						else find'  <$> RWS.gets (staticChain . envStack)
					where
						findDeep  _ [] =  Nothing
						findDeep  stack (x:xs) = Map.lookup name (symtableValues x) `mplus` maybe (findDeep stack xs) (aux stack <=< Map.lookup name) (symtableEnv x)
						aux s n = Map.lookup name $ symtableValues $ head $ dropWhile ((n/=) . symtableStackPos) s
						find'  = foldr (mplus . Map.lookup name . symtableValues ) Nothing


assignStatic name value  =  RWS.asks deepBinding >>= \x -> if x
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
									(Nothing,_)	-> x : next assign (symtableLexicalParent x) xs
									(Just _,m)	-> x{symtableValues=m}:xs
						assign' = insertLookupWithKey (\_ a old -> if checktype a old then a else error ("expected" ++ (show .fst) old ++ " got " ++ (show .fst) a)) name value
						next f p  = (\(x,y) -> x ++ f y) . span (\n -> symtableName n /= p)




{-emptySymtable ::  String -> String -> SymbolTable-}
emptySymtable :: Maybe (Map String Integer) -> Integer -> String -> String -> SymbolTable
emptySymtable =  SymbolTable Map.empty 

newScope name parent env=  stackPos >>=(\n ->  RWS.modify (\x -> x{envStack= emptySymtable env n name  parent : envStack x,envScope=1+envScope x}) ) 
			where
				stackPos = RWS.gets envScope 

exitScope :: RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
exitScope	= RWS.modify (\x -> x{envStack = tail (envStack x) ,envScope=envScope x-1})

insertSymbol name val binds = do{
						env<-St.get;
						let x = insert' name val' (envStack env) in
						St.put $ env{envStack=x}
					}
			where
				insert' a b (x:xs) = x{symtableValues=insert a b (symtableValues x)} : xs
				val'  = case val of
					(t,ValFun a b c n _) -> (t,ValFun a b c n binds)
					otherwise -> val

assignSymbol :: String-> (Type, SymbolValue)-> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
assignSymbol name val = RWS.asks assignScope >>= (\f -> f name val)

lookupSymbol :: String-> RWS.RWST ScopeConfig Log Environment (Either EvalError) (Type, SymbolValue)
lookupSymbol name = RWS.asks lookupScope >>= (\f -> f name) >>= maybe (RWS.lift $ Left $ NotInScope name) return

getCurrentStack :: RWS.RWST ScopeConfig Log Environment (Either EvalError) String
getCurrentStack = RWS.gets (symtableName . head . envStack)



giveType ::  SymbolValue -> (Type, SymbolValue)
giveType val@(ValInt _) = (TInt,val)
giveType val@(ValFun{}) = (TFunct,val)
giveType Uninitialized = error "\n Variable is unitialized" 

newEnvironment ::  Environment
newEnvironment = Environment [emptySymtable Nothing (-1)  "Global" ""]  0

printVal ::  Monad m => SymbolValue -> m SymbolValue
printVal x = trace (showValue x) (return Void)
		where
			trace s v =	let str = ( unsafePerformIO . putStr) s
						in deepseq str $  return str >> v

printValLn ::  Monad m => SymbolValue -> m SymbolValue
printValLn x = trace (showValue x) (return Void)
		where 
			trace s v =	let str = ( unsafePerformIO . putStrLn) s
						in deepseq str $  return str >> v


showValue ::  SymbolValue -> String
showValue (ValBool b) = show b
showValue (ValInt n) = show n
showValue (ValString s) = s
showValue (Void	) = "Void"

evalProg :: ScopeConfig -> [Stmt] ->Either EvalError (SymbolValue, Environment, Log)
evalProg conf block = RWS.runRWST (evalBlock (Block 0 block) "Global") conf newEnvironment

eval :: Stmt-> RWS.RWST ScopeConfig Log Environment (Either EvalError) SymbolValue
eval (CallPrint e) = evalType e >> evalExpr e >>= printVal
eval (CallPrintLn e) = evalType e >> evalExpr e >>= printValLn
eval (DeclVar name typ) = insertSymbol name (typ,Uninitialized) Nothing >> return Void
eval (DeclFunct name arg typ block) = getCurrentStack >>= (\x -> insertSymbol name (TFunct, ValFun arg typ block x Nothing) Nothing) >>  return Void
eval (Assign name e) =lookupSymbol name >>= (\(t,_) -> (==t) <$> evalType e >> (,)t<$> evalExpr e) >>= assignSymbol name >>return Void
eval (CallProc name arg) = evalType (CallFunct name arg) >> lookupSymbol name >>= 
										(\x -> case x of
											(TFunct,ValFun fargs _ block@(Block n _)  parent env) -> mapM evalExpr arg >>= insertArgs n parent fargs env  >> evalBlock block name >>= (\ret -> exitScope >> return ret)
											(t,_) -> RWS.lift $ Left $ WrongType t TFunct 
										)
						where
							{-insertArgs ::String -> [(Ident,Type)] -> [SymbolValue] -> RWS.RWS ScopeConfig Log  Environment [()]-}
							insertArgs n parent fargs env args' = do {
								binds <- getBindings;
								newScope (name++"|"++show n++"|args") parent env;
								zipWithM (\(name,t) v-> insertSymbol name (t,v) (Just binds)) fargs args' 
								}
eval (ControlIf cond tblock fblock) = do
									(ValBool x) <- evalExpr cond;
									if x then evalBlock tblock "ifTbranch" else evalBlock fblock "ifFbranch"
eval (Return a) =liftM Exit (maybe (return Void)  evalExpr a)

evalBlock :: Block-> String -> RWS.RWST ScopeConfig Log Environment (Either EvalError) SymbolValue
evalBlock (Block n b) name =  getCurrentStack >>= flip (newScope (name++"|"++show n)) Nothing 
							>> eval' b >>= (\x-> exitScope >> return x)
		where
			eval' [] = return Void
			eval' (x:xs) = eval x >>=(\ret -> case ret of
					 								Void -> eval' xs
													Exit val -> return val
													otherwise -> return ret)


evalType :: Expr -> RWS.RWST ScopeConfig Log Environment (Either EvalError) Type
evalType (IntLiteral{}) = return TInt
evalType (BoolLiteral{}) = return TBool
evalType (StringLiteral{}) = return TString
evalType (Var n) = RWS.asks lookupScope >>= (\f -> f n) >>= maybe (RWS.lift $ Left $ NotInScope n) (return.fst)
evalType (UnaryExp op e) = evalType e >>= (\x -> if x==t' then return x else RWS.lift $ Left $ WrongType x t')
					where
						t' = case op of
								Negate -> TInt
								Not -> TBool
evalType (BinaryExp op a b) = evalType a >>= check >> evalType b >>= check 
					where
						check e = if e==opT then return resT else RWS.lift $ Left $ WrongType e opT
							where
								opT = case op of 
									Add -> TInt 
									Minus -> TInt 
									Mult -> TInt 
									Div -> TInt 
									Mod -> TInt 
									Or -> TBool
									And -> TBool
									LessTE -> TInt
									LessT -> TInt
									GreaterTE -> TInt
									GreaterT -> TInt
									Equals -> e
									NEquals -> e
								resT = case op of 
									Add -> TInt 
									Minus -> TInt 
									Mult -> TInt 
									Div -> TInt 
									Mod -> TInt 
									Or -> TBool
									And -> TBool
									LessTE -> TBool
									LessT -> TBool
									GreaterTE -> TBool
									GreaterT -> TBool
									Equals -> TBool
									NEquals -> TBool
evalType (CallFunct n callargs) = RWS.asks lookupScope >>= (\f -> f n) >>= maybe (RWS.lift $ Left $ NotInScope n) go
					where
						check (_,typ) exp = evalType exp >>= (\x -> if x==typ then return typ else RWS.lift $ Left $ WrongType x typ)
						go (TFunct,ValFun declargs t _ _ _) = when (callen /= declen) (RWS.lift $ Left $ LessArgs n callen declen) >>zipWithM check declargs callargs 
										>> return t
											where
												callen = length callargs
												declen = length declargs
						go (t,_) = RWS.lift $ Left $ WrongType t TFunct

evalExpr :: Expr -> RWS.RWST ScopeConfig Log Environment (Either EvalError) SymbolValue
evalExpr (IntLiteral n) = return $ ValInt n
evalExpr (BoolLiteral b) = return $ ValBool b
evalExpr (StringLiteral s) = return $ ValString s
evalExpr (Var n) = do{
					x <- RWS.asks lookupScope >>= (\f -> f n);
					case fromJust x of
						(_,Uninitialized) -> RWS.lift $ Left $ VariableNotInitialized n
						(_,m) -> return m
					}
evalExpr (UnaryExp op e) = evalExpr e >>= f' op
				where
					f' Negate (ValInt n) = return $ValInt (-n)
					f' Not (ValBool b) = return $ValBool (not b)
evalExpr (BinaryExp op aexp bexp) = f' op <$> evalExpr aexp <*> evalExpr bexp
				where
					f' Add (ValInt a) (ValInt b) = ValInt $ a + b
					f' Minus (ValInt a) (ValInt b) =ValInt $  a - b
					f' Mult (ValInt a) (ValInt b) =ValInt $  a * b
					f' Div (ValInt a) (ValInt b) =ValInt $  a `div` b
					f' Mod (ValInt a) (ValInt b) =ValInt $  a `mod` b
					f' And (ValBool a) (ValBool b) =ValBool $  a && b
					f' Or (ValBool a) (ValBool b) =ValBool $  a || b
					f' LessT (ValInt a) (ValInt b) =ValBool $  a < b
					f' LessTE (ValInt a) (ValInt b) =ValBool $  a <= b
					f' GreaterT (ValInt a) (ValInt b) =ValBool $  a > b
					f' GreaterTE (ValInt a) (ValInt b) =ValBool $  a >= b
					f' Equals a b = ValBool $ a == b
					f' NEquals a b = ValBool $ a /= b
evalExpr (CallFunct n args) = eval (CallProc n args)


{- debugging -}

{-
 -printEnv :: RWS.RWS ScopeConfig Log Environment SymbolValue
 -printEnv = (\x -> trace (show x) Void) <$> RWS.get
 -}


exec :: SourceName -> ScopeConfig -> IO (Either ParseError (Either EvalError (SymbolValue, Environment, Log)))
exec file mode = return . liftM (evalProg mode) . runParser prog 0 file  =<< readFile file
