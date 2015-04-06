module Parser (
			exec,
			execString,
			dynDeepScope,
			dynShallowScope,
			staticShallowScope,
			staticDeepScope,
			ScopeConfig,
			SymbolValue(..),
			Environment,
			Log,
			Logunit,
			logInst,
			logEnv,
			symtableValues,
			symtableName,
			envStack,
			staticChain
			) where
import Prelude hiding (foldr,concatMap)
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
import qualified Data.Sequence as S
{-import Debug.Trace-}


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


opTable ::  [[Operator Char st Expr]]
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
					symtableValues :: Map String (Integer,(Type,SymbolValue)),
					symtableEnv :: Maybe (Map String (Integer,Integer)),
					symtableStackPos :: Integer,
					symtableFramePointer :: Integer,
					symtableName :: String,
					symtableLexicalParent :: String
				}
				deriving Show

data SymbolValue =	Uninitialized
				|	ValInt Integer
				|	ValBool Bool 
				|	ValString String
				|	ValFun FunctArgs Type Block String (Maybe (Map String (Integer,Integer)))
				|	Void
				|	Exit SymbolValue
				{-deriving (Show,Eq)-}
				deriving (Eq)



instance Show SymbolValue where
	show = showValue


data Environment = Environment {
					envStack :: [SymbolTable],
					{-envCallStack  :: [(String,[SymbolTable])],-}
					envScope :: Integer
				}			
				deriving Show



data ScopeConfig = ScopeConfig {
					deepBinding :: Bool,
					staticScoping :: Bool,
					lookupScope :: String-> RWS.RWST ScopeConfig Log Environment (Either EvalError) (Maybe (Type,SymbolValue)),
					assignScope	:: String->(Type,SymbolValue) -> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
				}


data Logunit = Logunit {
			logInst::Stmt,
			logEnv::Environment
}
		deriving Show

type Log = S.Seq Logunit


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




getBindings :: RWS.RWST ScopeConfig Log Environment (Either EvalError) (Map String (Integer,Integer))
getBindings =  RWS.asks staticScoping >>= \x-> if x
				then  foldr go Map.empty <$>RWS.gets (staticChain .envStack)
				else  foldr go Map.empty <$>RWS.gets envStack
					where
						go = union . (\y -> fmap (\(n,_) -> (symtableStackPos y,n)) (symtableValues y) )
						{-go = ((union . Map.fromList) . (\y-> map (\z->(z,symtableStackPos y)) (keys $symtableValues y)))-}

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
lookupDyn :: String -> RWS.RWST ScopeConfig Log Environment (Either EvalError) (Maybe (Type, SymbolValue))
lookupDyn name = RWS.asks deepBinding >>= (\x -> if x
					then fmap snd.findDeep <$> RWS.gets envStack 
					else fmap snd . find' <$> RWS.gets envStack
				)
				where
					findDeep [] = Nothing 
					findDeep l@(x:xs) = Map.lookup name (symtableValues x) `mplus` maybe (findDeep xs) (aux l . fst <=< Map.lookup name) (symtableEnv x)
					aux l n = Map.lookup name $ symtableValues $  head $  dropWhile ( (n/=).symtableStackPos) l

					find'  = foldr (mplus . Map.lookup name .symtableValues) Nothing

assignDyn :: String -> (Type, SymbolValue) -> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
assignDyn name value  = RWS.asks deepBinding >>= (\x -> if x
					then RWS.modify (\y -> y{envStack=assignDeep $ envStack y})
					else RWS.modify (\y -> y{envStack=assign $ envStack y})
				)
				where
					assignDeep [] = error (name++" variable not in scope")
					assignDeep l@(x:xs) = case assign' (symtableValues x) of
									(Nothing,_) -> maybe (x:assignDeep xs) (aux l . fst . fromJust . Map.lookup name) (symtableEnv x)
									(Just _,m)	-> x{symtableValues = m} : xs
					aux l n = (\(x,y) -> x ++ assign y) $ break ((n==) . symtableStackPos) l
					assign [] = error (name++" variable not in scope")
					assign (x:xs) = case assign' (symtableValues x) of 
									(Nothing,_) -> x:assign xs
									(Just _,m) -> x{symtableValues=m}:xs
					assign' = insertLookupWithKey (\_ (_,a) (n,old) -> if checktype a old then (n,a) else error ("expected" ++ show (fst old) ++ " got " ++ show (fst a))) name (0,value)


{- static scope -}
lookupStatic :: String -> RWS.RWST ScopeConfig Log Environment (Either EvalError) (Maybe (Type, SymbolValue))
lookupStatic name = RWS.asks deepBinding >>= \x-> if x
						then (\x -> fmap snd . findDeep x ) <$> RWS.gets envStack <*> RWS.gets (staticChain . envStack)
						else fmap snd . find'  <$> RWS.gets (staticChain . envStack)
					where
						findDeep  _ [] =  Nothing
						findDeep  stack (x:xs) = Map.lookup name (symtableValues x) `mplus` maybe (findDeep stack xs) (aux stack . fst <=< Map.lookup name) (symtableEnv x)
						aux s n = Map.lookup name $ symtableValues $ head $ dropWhile ((n/=) . symtableStackPos) s
						find'  = foldr (mplus . Map.lookup name . symtableValues ) Nothing


assignStatic :: String -> (Type, SymbolValue) -> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
assignStatic name value  =  RWS.asks deepBinding >>= \b -> if b
						then RWS.modify (\x -> x{envStack=assignDeep $ envStack x})
						else RWS.modify (\x -> x{envStack=assign $ envStack x})
					where
						assignDeep [] = error (name ++ "variable not in scope")
						assignDeep l@(x:xs) = case assign' (symtableValues x) of
									(Nothing , _) -> maybe (x: next assignDeep (symtableLexicalParent x) xs) (aux l . fst . fromJust . Map.lookup name) (symtableEnv x)
									(Just _ , m)  -> x{symtableValues=m}:xs
						aux l n = (\(x,y) -> x ++ assign y) $ break ((n==) . symtableStackPos) l
						assign [] = error (name ++ "variable not in scope")
						assign (x:xs) = case assign' (symtableValues x) of
									(Nothing,_)	-> x : next assign (symtableLexicalParent x) xs
									(Just _,m)	-> x{symtableValues=m}:xs
						assign' = insertLookupWithKey (\_ (_,a) (n,old) -> if checktype a old then (n,a) else error ("expected" ++ (show .fst) old ++ " got " ++ (show .fst) a)) name (0,value)
						next f p  = (\(x,y) -> x ++ f y) . span (\n -> symtableName n /= p)




{-emptySymtable ::  String -> String -> SymbolTable-}
{-emptySymtable :: Maybe (Map String Integer) -> Integer -> String -> String -> SymbolTable-}
emptySymtable =  SymbolTable Map.empty 

newScope :: String -> String -> Maybe (Map String (Integer,Integer)) -> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
newScope name parent env= do
				pos <- stackPos
				fp <- stackSP
				RWS.modify (\x -> x{envStack= emptySymtable env pos fp name  parent : envStack x,envScope=1+envScope x}) 
			where
				stackPos = RWS.gets envScope 
				{-stackSP  = RWS.gets (head . envStack) >>= return . (\x -> (symtableFramePointer x) + fromIntegral (Map.size (symtableValues x)))-}
				stackSP  = (\x -> symtableFramePointer x + fromIntegral (Map.size (symtableValues x))) <$>  RWS.gets (head . envStack) 

exitScope :: RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
exitScope	= RWS.modify (\x -> x{envStack = tail (envStack x) ,envScope=envScope x-1})

insertSymbol :: String -> (Type, SymbolValue) -> Maybe (Map String (Integer,Integer)) -> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
insertSymbol name val binds = do
						env<-St.get
						sp <- stackSP
						let x = insert' name (sp,val') (envStack env) in
							St.put $ env{envStack=x}
					
			where
				insert' _ _ [] = error "inserting symbol on empty environment"
				insert' a b (x:xs) = x{symtableValues=insert a b (symtableValues x)} : xs
				stackSP  = (\x -> symtableFramePointer x + fromIntegral (Map.size (symtableValues x))) <$>  RWS.gets (head . envStack) 
				val'  = case val of
					(t,ValFun a b c n _) -> (t,ValFun a b c n binds)
					_ -> val

assignSymbol :: String-> (Type, SymbolValue)-> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
assignSymbol name val = RWS.asks assignScope >>= (\f -> f name val)

lookupSymbol :: String-> RWS.RWST ScopeConfig Log Environment (Either EvalError) (Type, SymbolValue)
lookupSymbol name = RWS.asks lookupScope >>= (\f -> f name) >>= maybe (RWS.lift $ Left $ NotInScope name) return

getCurrentStack :: RWS.RWST ScopeConfig Log Environment (Either EvalError) String
getCurrentStack = RWS.gets (symtableName . head . envStack)


newEnvironment ::  Environment
newEnvironment = Environment [emptySymtable Nothing (-1) 0  "Global" ""]  0 

printVal ::  Monad m => SymbolValue -> m SymbolValue
printVal x = trace' (showValue x) (return Void)
		where
			trace' s v = let str = ( unsafePerformIO . putStr) s
						in deepseq str v

printValLn ::  Monad m => SymbolValue -> m SymbolValue
printValLn x = trace' (showValue x) (return Void)
		where 
			trace' s v = let str = ( unsafePerformIO . putStrLn) s
						in deepseq str v

showValue ::  SymbolValue -> String
showValue (ValBool b) = show b
showValue (ValInt n) = show n
showValue (ValString s) = s
showValue (Void	) = "Void"
showValue (Uninitialized) = "Uninitialized"
showValue Exit{} = "exit"
showValue (ValFun fargs typ _ _ _) = "("++ concatMap ((++",") . show . snd) fargs ++"):"++show typ

evalProg :: ScopeConfig -> [Stmt] ->Either EvalError (SymbolValue, Environment, Log)
evalProg conf block = RWS.runRWST (evalBlock (Block 0 block) "Global") conf newEnvironment

eval :: Stmt-> RWS.RWST ScopeConfig Log Environment (Either EvalError) SymbolValue
eval inst@(CallPrint e) = do
				RWS.get >>= RWS.tell . S.singleton . Logunit inst
				evalType e
				evalExpr e >>= printVal
eval inst@(CallPrintLn e) = do
				RWS.get >>= RWS.tell . S.singleton . Logunit inst
				evalType e
				evalExpr e >>= printValLn
eval inst@(DeclVar name typ) =do
				RWS.get >>= RWS.tell . S.singleton . Logunit inst
				insertSymbol name (typ,Uninitialized) Nothing
				return Void
eval inst@(DeclFunct name arg typ block) = do
				RWS.get >>= RWS.tell . S.singleton . Logunit inst
				getCurrentStack >>= (\x -> insertSymbol name (TFunct, ValFun arg typ block x Nothing) Nothing)
				return Void
eval inst@(Assign name e) = do
				RWS.get >>= RWS.tell . S.singleton . Logunit inst
				(t1,_)<-lookupSymbol name
				t2<- evalType e 
				if t1/=t2
					then RWS.lift $ Left $ WrongType t1 t2
					else do 
						x <- (,)t2<$> evalExpr e 
						assignSymbol name x
						return Void 
eval inst@(CallProc name arg) = do
				RWS.get >>= RWS.tell . S.singleton . Logunit inst
				evalType (CallFunct name arg)
				lookupSymbol name >>= (\x -> case x of
					(TFunct,ValFun fargs _ block@(Block n _)  parent env) -> do 
							mapM evalExpr arg >>= insertArgs n parent fargs env
							ret <- evalBlock block name
							exitScope
							return ret
					(t,_) -> RWS.lift $ Left $ WrongType t TFunct 
									)	
						where
							insertArgs n parent fargs env args' = do {
								binds <- getBindings;
								newScope (name++"|"++show n++"|args") parent env;
								zipWithM (\(nam,t) v-> insertSymbol nam (t,v) (Just binds)) fargs args' 
								}
eval inst@(ControlIf cond tblock fblock) = do
									RWS.get >>= RWS.tell . S.singleton . Logunit inst;
									(ValBool x) <- evalExpr cond;
									if x then evalBlock tblock "ifTbranch" else evalBlock fblock "ifFbranch"
eval inst@(Return a) = RWS.get >>= RWS.tell . S.singleton . Logunit inst >>liftM Exit (maybe (return Void)  evalExpr a)

evalBlock :: Block-> String -> RWS.RWST ScopeConfig Log Environment (Either EvalError) SymbolValue
evalBlock (Block n b) name =  getCurrentStack >>= flip (newScope (name++"|"++show n)) Nothing 
							>> eval' b >>= (\x-> exitScope >> return x)
		where
			eval' [] = return Void
			eval' (x:xs) = eval x >>=(\ret -> case ret of
					 								Void -> eval' xs
													Exit val -> return val
													_ -> return ret)


evalType :: Expr -> RWS.RWST ScopeConfig Log Environment (Either EvalError) Type
evalType (IntLiteral{}) = return TInt
evalType (BoolLiteral{}) = return TBool
evalType (StringLiteral{}) = return TString
evalType (Var n) = RWS.asks lookupScope >>= 
					(\f -> f n) >>= 
					maybe (RWS.lift $ Left $ NotInScope n) (return.fst)
evalType (UnaryExp op e) = evalType e >>= 
							(\x -> if x==t'
									then return x
									else RWS.lift $ Left $ WrongType x t')
					where
						t' = case op of
								Negate -> TInt
								Not -> TBool
evalType (BinaryExp op a b) = do 
					evalType a >>= check
					evalType b >>= check 
					where
						check e = if e==opT then return resT else RWS.lift $ Left $ WrongType e opT
							where
								(opT, resT) = case op of 
									Add -> (TInt , TInt)
									Minus -> (TInt  , TInt)
									Mult -> (TInt , TInt )
									Div -> (TInt , TInt )
									Mod -> (TInt , TInt )
									Or -> (TBool ,TBool)
									And -> (TBool ,TBool)
									LessTE -> (TInt ,TBool)
									LessT -> (TInt ,TBool)
									GreaterTE -> (TInt ,TBool)
									GreaterT ->( TInt ,TBool)
									Equals -> (e ,TBool)
									NEquals ->( e ,TBool)
evalType (CallFunct n callargs) = RWS.asks lookupScope >>=
								 (\f -> f n) >>=
								 maybe (RWS.lift $ Left $ NotInScope n) go
					where
						check (_,typ) ex = evalType ex >>= 
							(\x -> if x==typ 
									then return typ
									else RWS.lift $ Left $ WrongType x typ)
						go (TFunct,ValFun declargs t _ _ _) = do
								when (callen /= declen) (RWS.lift $ Left $ LessArgs n callen declen)
								zipWithM_ check declargs callargs 
								return t
									where
										callen = length callargs
										declen = length declargs
						go (t,_) = RWS.lift $ Left $ WrongType t TFunct

evalExpr :: Expr -> RWS.RWST ScopeConfig Log Environment (Either EvalError) SymbolValue
evalExpr (IntLiteral n) = return $ ValInt n
evalExpr (BoolLiteral b) = return $ ValBool b
evalExpr (StringLiteral s) = return $ ValString s
evalExpr (Var n) = do
					x <- RWS.asks lookupScope >>= (\f -> f n)
					case fromJust x of
						(_,Uninitialized) -> RWS.lift $ Left $ VariableNotInitialized n
						(_,m) -> return m
evalExpr (UnaryExp op e) = evalExpr e >>= f' op
				where
					f' Negate (ValInt n) = return $ValInt (-n)
					f' Not (ValBool b) = return $ValBool (not b)
					f' _ _  = error "unsupported unary expression"
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
					f' _ _ _ = error "unsupported binary expression"
evalExpr (CallFunct n cargs) = eval (CallProc n cargs)


{- debugging -}

{-
 -printEnv :: RWS.RWS ScopeConfig Log Environment SymbolValue
 -printEnv = (\x -> trace (show x) Void) <$> RWS.get
 -}

execString ::  ScopeConfig -> String -> Either ParseError (Either EvalError (SymbolValue, Environment, Log))
execString mode = liftM (evalProg mode) . runParser prog 0 "Input"

exec :: SourceName -> ScopeConfig -> IO (Either ParseError (Either EvalError (SymbolValue, Environment, Log)))
exec file mode = return . liftM (evalProg mode) . runParser prog 0 file  =<< readFile file
