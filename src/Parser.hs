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
			Logunit(..),
			symtableValues,
			symtableName,
			envStack,
			wGetLine,
			staticChain
			) where
import Prelude hiding (foldr,concatMap,concat)
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import qualified Text.ParserCombinators.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Data.Map as Map hiding (map,foldr)
import Data.Functor
import Data.List (intercalate,isPrefixOf)
import Data.Maybe
import Data.Foldable
import Control.Applicative
import qualified Control.Monad.RWS as RWS
import Control.Monad
import Control.DeepSeq
import System.IO.Unsafe
import qualified Data.Sequence as S
import Debug.Trace
import Data.Monoid

flagDebug ::  Bool
flagDebug = False

type Ident = String
type FunctArgs = [(Ident,Type)]

data Block = Block Int Integer [Stmt]
		deriving Show
	
instance Eq Block where
	(Block _ a _) == (Block _ b _) = a == b

class WithLine a where
	wGetLine :: a -> Int


data Stmt =   DeclVar Int Ident Type
			| DeclFunct Int Ident FunctArgs Type Block 
			| Assign Int Ident Expr
			| CallProc Int Ident [Expr]
			| ControlIf Int Expr Block Block
			| CallPrint Int Expr
			| CallPrintLn Int Expr
			| Return Int (Maybe Expr)
			| EndProg
	{-deriving Show-}

instance Show Stmt where
	show = stmtShow

instance WithLine Stmt where
	wGetLine (DeclVar n _ _) = n
	wGetLine (DeclFunct n _ _ _ _) = n
	wGetLine (Assign n _ _) = n
	wGetLine (CallProc n _ _) = n
	wGetLine (ControlIf n _ _ _) = n
	wGetLine (CallPrint n  _) = n
	wGetLine (CallPrintLn n  _) = n
	wGetLine (Return n _ ) = n
	wGetLine (EndProg) = -1

stmtShow ::  Stmt -> String
stmtShow (DeclVar l name typ) = show l ++ ": var "++name++" : "++ show typ
stmtShow (DeclFunct l name fargs typ _) = show l ++ ": sub "++name++ fargs'++" : "++ show typ ++" {...}"
	where
		fargs' = "(" ++ intercalate "," (map (\(x,y) -> x ++":" ++ show y) fargs) ++ ")"
stmtShow (Assign l name expression) = show l ++ ": "++name++" = "++ show expression
stmtShow (CallProc l name fargs) = show l ++ ": "++name++" ("++ intercalate "," (map show fargs) ++ ") "
stmtShow (ControlIf l expression _ _) = show l ++ ": if ("++show expression++") "
stmtShow (CallPrint l expression) = show l ++ ": print("++show expression++") "
stmtShow (CallPrintLn l expression) = show l ++ ": printLn("++show expression++") "
stmtShow (Return l expression) = show l ++ ": return "++show expression
stmtShow (EndProg) = "Program ended"

data Type =   TInt
			| TFunct
			| TBool
			| TString
			| TVoid
			deriving (Eq)
			{-deriving (Show,Eq)-}

instance Show Type where
	show = typeShow	
	
typeShow ::  Type -> String
typeShow (TInt) = "int"
typeShow (TFunct) = "sub"
typeShow (TBool) = "bool"
typeShow (TString) = "String"
typeShow (TVoid) = "Void"

data Expr =   Var Int Ident
			| IntLiteral Int Integer
			| BoolLiteral Int Bool
			| StringLiteral Int String
			| CallFunct Int Ident [Expr]
			| UnaryExp Int UnaryOP Expr
			| BinaryExp Int BinaryOP Expr Expr
			{-deriving Show-}

instance Show Expr where
	show = exprShow

instance WithLine Expr where
	wGetLine (Var n  _) = n
	wGetLine (IntLiteral n  _) = n
	wGetLine (BoolLiteral n  _) = n
	wGetLine (StringLiteral n  _) = n
	wGetLine (CallFunct n _ _) = n
	wGetLine (UnaryExp n _ _) = n
	wGetLine (BinaryExp n _ _ _) = n

exprShow ::  Expr -> Ident
exprShow (Var _ name) = name
exprShow (IntLiteral _ n) = show n
exprShow (BoolLiteral _ b) = show b
exprShow (StringLiteral _ s) = s
exprShow (CallFunct _ name fargs) = name ++ "(" ++ intercalate "," (map show fargs) ++ ")"
exprShow (UnaryExp _ op expr') = "(" ++ show op ++" "++ show expr' ++ ")"
exprShow (BinaryExp _ op expr1 expr2 ) = "(" ++show expr1 ++" "++ show op ++" "++ show expr2 ++ ")"

data UnaryOP = Negate
			|  Not
			{-deriving(Show)-}

instance Show UnaryOP where
	show = uopShow

uopShow ::  UnaryOP -> String
uopShow (Negate) = "-"
uopShow (Not) = "!"

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
			{-deriving Show-}

instance Show BinaryOP where
	show = bopShow

bopShow ::  BinaryOP -> String
bopShow (Add) = "+"
bopShow (Mult) = "*"
bopShow (Div) = "/"
bopShow (Minus) = "-"
bopShow (Mod) = "%"
bopShow (And) = "&&"
bopShow (Or) = "||"
bopShow (LessT) = "<"
bopShow (LessTE) = "<="
bopShow (GreaterT) = ">"
bopShow (GreaterTE) = ">="
bopShow (Equals) = "=="
bopShow (NEquals) = "!="



-- Tokens

dymanikStyle ::  LanguageDef st
dymanikStyle = emptyDef { 
		commentStart = "/*",
		commentEnd 	= "*/",
		commentLine = "//",
		identStart = letter,
		identLetter = alphaNum <|> char '_',
		reservedOpNames = ["+","-","/","*","=","&&","||","<",">",
									"<=",">=","==","!=","!", ":"],
		reservedNames = ["sub","return","proc","if","else","var",
							"int","bool", "Void", "print", "printLn" ]
		}

lexer = Tok.makeTokenParser dymanikStyle
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
--Parse the whole program
prog = whiteSpace *> manyTill stmt eof

-- Statments blocks
stmts = Block <$> (sourceLine <$> getPosition) 
				<*> (updateState (+1) >> getState ) 
				<*> many stmt


stmt =  declFunct
	<|> funReturn
	<|> controlIf
	<|> (declVar
	<|> callPrintLn
	<|> callPrint
	<|> (identifier >>=( \x-> flip CallProc x <$> (sourceLine <$> getPosition) <*> parens args 
						<|> (flip Assign x <$>  (sourceLine <$> getPosition) <*> (reservedOp "=" *> expr)
						<?> "Subroutine Call o Assignment"))) 
	) <* semi

funReturn = Return <$> (sourceLine <$> getPosition) 
					<*> (reserved "return" *> optionMaybe expr <* semi) 
					<?> "return"

--Variable Declaration
declVar =  DeclVar <$>  (sourceLine <$> getPosition) 
					<*> (reserved "var" *> identifier) 
					<*> (varType) 
					<?> "variable declaration"

varType =  reservedOp ":" *>(
			(reserved "int" *> return TInt)
		<|> (reserved "sub" *> return TFunct)
		<|> (reserved "bool" *> return TBool))
		<?> "type declaration"

funcType =  reservedOp ":" *>(
			(reserved "int" *> return TInt)
		<|> (reserved "sub" *> return TFunct)
		<|> (reserved "bool" *> return TBool)
		<|> (reserved "Void" *> return TVoid))
		<?> "type declaration"

--Function Declaration
declFunct = DeclFunct <$>  (sourceLine <$> getPosition) 
						<*> (reserved "sub" *> identifier)
						<*> parens functdeclargs 
						<*> (funcType) 
						<*> braces stmts 
						<?> "function declaration"

functdeclargs = commaSep ((,) <$> identifier <*> ( varType))

callPrint = CallPrint <$>  (sourceLine <$> getPosition) 
						<*> (reserved "print" *> parens expr)

callPrintLn = CallPrintLn <$>  (sourceLine <$> getPosition) 
						<*> (reserved "printLn" *> parens expr)

args = commaSep expr


controlIf = ControlIf <$>  (sourceLine <$> getPosition)
					<*> (reserved "if" *> parens expr)
					<*> braces stmts
					<*> (reserved "else" *> braces stmts 
							<|> Block <$> (sourceLine <$> getPosition) 
										<*> getState <*> return [])

--Expresion Parsers


opTable ::  [[Operator Char st Expr]]
opTable = [[ prefix "-" (flip UnaryExp Negate),
					prefix "!" (flip UnaryExp Not)],
			[ binary "*" (flip BinaryExp Mult) AssocLeft,
					binary "/" (flip BinaryExp Div) AssocLeft,
					binary "%" (flip BinaryExp Mod) AssocLeft ],
			[ binary "+" (flip BinaryExp Add) AssocLeft, 
					binary "-" (flip BinaryExp Minus) AssocLeft],
			[ binary "<" (flip BinaryExp LessT) AssocLeft,
					binary "<=" (flip BinaryExp LessTE) AssocLeft,
					binary ">" (flip BinaryExp GreaterT) AssocLeft,
					binary ">=" (flip BinaryExp GreaterTE) AssocLeft],
			[ binary "==" (flip BinaryExp Equals) AssocLeft,
					binary "!=" (flip BinaryExp NEquals) AssocLeft],
			[ binary "&&" (flip BinaryExp And) AssocLeft],
			[ binary "||" (flip BinaryExp Or) AssocLeft]
		  ]
		  where
		  	prefix op f  = Prefix (reservedOp op >> f <$> (sourceLine <$> getPosition)) 
			binary op f  = Infix (reservedOp op >> f <$> (sourceLine <$> getPosition)) 


expr =  buildExpressionParser opTable term

term =  parens expr
	<|> IntLiteral <$> (sourceLine <$> getPosition) <*> integer
	<|> (reserved "true" >> BoolLiteral <$> (sourceLine <$> getPosition)
										<*> return True)
	<|> (reserved "false" >> BoolLiteral <$> (sourceLine <$> getPosition) 
										<*> return False)
	<|> StringLiteral <$> (sourceLine <$> getPosition) <*> stringLiteral
	<|> (identifier >>=( \x-> flip CallFunct x <$>  (sourceLine <$> getPosition)
										<*> parens args 
							<|> Var <$> (sourceLine <$> getPosition) 
										<*> return x)) 
	<?> "an expression"

--------------evaluation --------

data SymbolTable = SymbolTable {
					symtableValues :: Map Ident (Int,(Type,[SymbolValue])),
					symtableEnv :: Maybe (Map String (Integer,Int)),
					symtableStackPos :: Integer,
					symtableFramePointer :: Int,
					symtableName :: String,
					symtableLexicalParent :: (String,Int)
				}
				deriving Show

data SymbolValue =	Uninitialized
				|	ValInt Integer
				|	ValBool Bool 
				|	ValString String
				|	ValFun{
						valFunargs::FunctArgs,
						valFunType::Type,
						valFunBlock::Block,
						valFunParent::(String,Int),
						valFunEnv::Maybe (Map String (Integer,Int))
					}
				|	Void
				|	Exit SymbolValue
				{-deriving (Show,Eq)-}
				deriving (Eq)

instance NFData SymbolValue where
	rnf a = a `seq` ()

instance Show SymbolValue where
	show = showValue


data Environment = Environment {
					envStack :: [SymbolTable],
					envRetType :: [Type],
					envScope :: Integer
				}			
				deriving Show



data ScopeConfig = ScopeConfig {
					deepBinding :: Bool,
					staticScoping :: Bool,
					lookupScope :: Ident-> RWS.RWST ScopeConfig Log Environment (Either EvalError) (Maybe (Type,SymbolValue)),
					assignScope	:: Ident->(Type,SymbolValue) -> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
				}


data Logunit = Logunit {
			logStatic::Bool,
			logDeep::Bool,
			logInst::Stmt,
			logEnv::Environment
}
		deriving Show

type Log = S.Seq Logunit


{-- EXCEPTION --}
data EvalError =  NotInScope Int Ident
				| VariableNotInitialized Int Ident
				| WrongType Int Type Type
				| LessArgs Int Ident Int Int
				| AlreadyDeclared Int Ident

instance Show EvalError where
	show (NotInScope l n) = show l ++": identifier "++show n++" not in scope"
	show (VariableNotInitialized l n) = show l ++  ": variable "++show n++" has not been initialized"
	show (WrongType l a b) = show l ++ ": unexpected "++show a++" expected "++show b
	show (LessArgs l n a b) = show l ++ ": called sub "++show n++" with "++show a++" arguments expected "++ show b
	show (AlreadyDeclared l n) = show l ++ ": the name  "++show n++" is already declared in this scope."


{-getFreeVariables :: FunctArgs -> Block -> [String]-}

{-getFreeVariables fargs (Block _ _ blk) = snd $ head $ map (go) blk-}
getFreeVariables ::  FunctArgs -> Block -> Map Ident Ident
getFreeVariables fargs (Block _ _ blk) = flip Map.difference (Map.fromList fargs) $ snd $ Data.Foldable.foldl (\(decl,acc) (new,free) -> (mappend decl new, mappend (difference free decl) (acc) )) (mempty,mempty) $ map go blk
				where
					go :: Stmt -> (Map Ident Ident,Map Ident Ident)
					go (DeclVar _ ident _) = (Map.singleton ident ident,mempty)
					go (DeclFunct _ ident fargs' _ blok) = (Map.singleton ident ident,getFreeVariables fargs' blok)
					go (Assign _ ident expr') = (mempty,mappend (Map.singleton ident ident) (goexpr expr'))
					go (CallProc _ ident args') = (mempty,mappend (Map.singleton ident ident) (foldMap goexpr args'))
					go (ControlIf _ bexpr tblk fblk) = (mempty,(goexpr bexpr) `mappend` (getFreeVariables [] tblk) `mappend` (getFreeVariables [] fblk))
					go (CallPrint _ expr') = (mempty, goexpr expr')
					go (CallPrintLn _ expr') = (mempty, goexpr expr')
					go (Return _ expr') = (mempty, maybe mempty goexpr expr')
					go (EndProg) = error "endProg is a dummy value"

					goexpr (Var _ ident) = Map.singleton ident ident
					goexpr (IntLiteral{} ) = mempty
					goexpr (BoolLiteral{}) = mempty
					goexpr (StringLiteral{}) = mempty
					goexpr (CallFunct _ ident args') = mappend (Map.singleton ident ident) (foldMap goexpr args')
					goexpr (UnaryExp _ _ expr') = goexpr expr'
					goexpr (BinaryExp _ _ expr1 expr2) = mappend (goexpr expr1) (goexpr expr2)

addLog :: (Applicative m,Monad m) => Stmt -> RWS.RWST ScopeConfig Log Environment m ()
addLog inst = RWS.tell =<< (\a b c d -> S.singleton $ Logunit a b c d ) <$> RWS.asks (staticScoping) <*> RWS.asks deepBinding <*> return inst <*>  RWS.get
{-Logunit <$> RWS.asks (staticScoping) <*> RWS.asks deepBinding <*> return inst <*> RWS.get-}



getBindings :: (String,Int) -> RWS.RWST ScopeConfig Log Environment (Either EvalError) (Map String (Integer,Int))
getBindings (parent,off) =  RWS.asks staticScoping >>= \x-> if x
				then  foldr go Map.empty <$>RWS.gets (staticChain . g .  dropWhile ((/= parent).symtableName) . envStack)
				else  foldr go Map.empty <$>RWS.gets (dropWhile (\y->"args.of." `isPrefixOf` symtableName y) . envStack)
					where
						go = union . (\y -> fmap (\(n,_) -> (symtableStackPos y,n)) (symtableValues y) )
						g [] = error "empty stack / Parent not Found"
						g (y:ys) = y{symtableValues=cleanMap y} : ys
						cleanMap y = Map.filter (\(n,_) ->   n <= (off + symtableFramePointer y)) (symtableValues y)

dynShallowScope ::  ScopeConfig
dynShallowScope = ScopeConfig False False lookupDyn assignDyn

dynDeepScope ::  ScopeConfig
dynDeepScope = ScopeConfig True False lookupDyn assignDyn

staticShallowScope ::  ScopeConfig
staticShallowScope = ScopeConfig False True lookupStatic assignStatic

staticDeepScope ::  ScopeConfig
staticDeepScope = ScopeConfig True True lookupStatic assignStatic

staticChain :: [SymbolTable] -> [SymbolTable]
staticChain [] = []
staticChain (x:xs) = x : f xs
				where 
					f = staticChain . g . dropWhile (\n-> symtableName n /= fst (symtableLexicalParent x))
					g [] = []
					g (y:ys) = y{symtableValues=cleanMap y} : ys
					cleanMap y = Map.filter (\(n,_) ->  fromIntegral n <= (fromIntegral (snd $ symtableLexicalParent x) + symtableFramePointer y)) (symtableValues y)


{- dinamic scopes -}
lookupDyn :: String -> RWS.RWST ScopeConfig Log Environment (Either EvalError) (Maybe (Type, SymbolValue))
lookupDyn name = RWS.asks deepBinding >>= (\x -> if x
					then fmap (\(_,(t, v:_ )) -> (t,v)) . findDeep <$> RWS.gets envStack 
					else fmap (\(_,(t, v:_ )) -> (t,v)) . find' <$> RWS.gets envStack
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
					assign' = insertLookupWithKey (\_ (_,(tnew,a)) (n,(told,old)) -> if tnew==told then (n,(told,a++old)) else error ("expected" ++ show told ++ " got " ++ show tnew)) name value'
					value' = (\(b,c) -> (0,(b,[c]))) value


{- static scope -}
lookupStatic :: String -> RWS.RWST ScopeConfig Log Environment (Either EvalError) (Maybe (Type, SymbolValue))
lookupStatic name = RWS.asks deepBinding >>= \x-> if x
						then (\y -> fmap (\(_,(t, z:_ )) -> (t,z)) . findDeep y ) <$> RWS.gets envStack <*> RWS.gets (staticChain . envStack)
						else fmap (\(_,(t, y:_ )) -> (t,y)) . find'  <$> RWS.gets (staticChain . envStack)
					where
						findDeep  _ [] =  Nothing
						findDeep  stack (x:xs) = Map.lookup name (symtableValues x) `mplus` maybe (findDeep stack xs) (aux stack . fst <=< Map.lookup name) (symtableEnv x)
						aux s n = Map.lookup name $ symtableValues $ head $ dropWhile ((n/=) . symtableStackPos) s
						find'  = foldr (mplus . Map.lookup name . symtableValues ) Nothing


assignStatic :: String -> (Type, SymbolValue) -> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
assignStatic name value  =  do
						b<-RWS.asks deepBinding 
						if b
							then RWS.modify (\x -> x{envStack=assignDeep (-1) $ envStack x})
							else RWS.modify (\x -> x{envStack=assign (-1) $ envStack x})
					where
						assignDeep _ [] = error (name ++ "variable not in scope")
						assignDeep parent l@(x:xs) = case clean parent x of
									(False, _) -> maybe (x: next assignDeep (symtableLexicalParent x) xs) (aux l . fst . fromJust . Map.lookup name) (symtableEnv x)
									(True, m)  -> x{symtableValues=m}:xs
						aux l n = (\(x,y) -> x ++ assign (-1) y) $ break ((n==) . symtableStackPos) l
						assign _ [] = error (name ++ "variable not in scope")
						assign parent (x:xs) = case clean parent x of
									(False, _)	-> x : next assign (symtableLexicalParent x) xs
									(True,m)	-> x{symtableValues=m}:xs
						clean (-1) table = (member name $ symtableValues table , assign' $ symtableValues table)
						clean (offset) table = (member name $ Map.filter (\(n,_) -> n <= offset + symtableFramePointer table) (symtableValues table) ,assign' $ symtableValues table) 
						assign' = insertWithKey (\_ (_,(tnew,a)) (n,(told,old)) -> if tnew==told then (n,(told,a++old)) else error ("expected" ++ show told ++ " got " ++ show tnew)) name value'
						next f (p,off)  = (\(x,y) -> x ++ f off y) . span (\n -> symtableName n /= p)
						value' = (\(b,c) -> (0,(b,[c]))) value




emptySymtable :: Maybe (Map String (Integer, Int))-> Integer -> Int -> String -> (String,Int) -> SymbolTable
emptySymtable =  SymbolTable Map.empty 

newScope :: String -> (String,Int) -> Maybe (Map String (Integer,Int)) -> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
newScope name (parent,off) env= do
				pos <- stackPos
				fp <- stackSP
				off' <- if off<0 then offset else return off
				RWS.modify (\x -> x{envStack= emptySymtable env pos fp name  (parent,off') : envStack x,envScope=1+envScope x}) 
			where
				stackPos = RWS.gets envScope 
				offset =  RWS.gets ( Map.size . symtableValues . head . envStack)
				stackSP  = (\x -> symtableFramePointer x + Map.size (symtableValues x)) <$>  RWS.gets (head . envStack) 

exitScope :: RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
exitScope	= RWS.modify (\x -> x{envStack = tail (envStack x) ,envScope=envScope x-1})

insertSymbol :: Int->Bool -> String -> (Type, SymbolValue)  -> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
insertSymbol lineno toBind name val  = do
						env<- RWS.get
						sp <- stackSP
						val' <- getValWithBindings 
						if notMember name (symtableValues $ head $ envStack env)
							then do
								let x = insert' name (sp,val') (envStack env)
								RWS.put $ env{envStack=x}
							else RWS.lift $ Left $ AlreadyDeclared lineno name
			where
				insert' _ _ [] = error "inserting symbol on empty environment"
				insert' a b (x:xs) = x{symtableValues=insert a b (symtableValues x)} : xs
				stackSP  = (\x -> symtableFramePointer x + fromIntegral (Map.size (symtableValues x))) <$>  RWS.gets (head . envStack) 
				getValWithBindings = case val of
					(t,vf@(ValFun{valFunEnv=env})) -> if toBind
									then do
										binds <- flip intersection (getFreeVariables (valFunargs vf) (valFunBlock vf)) <$> getBindings (valFunParent vf)
										return (t,[vf{valFunEnv=env `mplus` Just binds}])
									else return (t,[vf{valFunEnv=Nothing}])
					(t,v) -> return (t,[v])

assignSymbol :: String-> (Type, SymbolValue)-> RWS.RWST ScopeConfig Log Environment (Either EvalError) ()
assignSymbol name val = RWS.asks assignScope >>= (\f -> f name val)

lookupSymbol :: Int -> String-> RWS.RWST ScopeConfig Log Environment (Either EvalError) (Type, SymbolValue)
lookupSymbol lineno name = RWS.asks lookupScope >>= (\f -> f name) >>= maybe (RWS.lift $ Left $ NotInScope lineno name) return

getCurrentStack :: RWS.RWST ScopeConfig Log Environment (Either EvalError) (String,Int)
getCurrentStack = RWS.gets ((\y -> (symtableName y, Map.size $ symtableValues y)) . head . envStack)


newEnvironment ::  Environment
newEnvironment = Environment {
						envStack=[emptySymtable Nothing (-1) 0  "Global" ("",0)],
						envScope=0,
						envRetType=[TVoid]
				}

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
showValue ValFun{valFunargs=fargs,valFunType=typ} = "("++ concatMap ((++",") . show . snd) fargs ++"):"++show typ

valueType :: SymbolValue -> Type
valueType (Void{}) = TVoid
valueType (ValFun{}) = TFunct
valueType (ValBool{}) = TBool
valueType (ValInt{}) = TInt
valueType (ValString{}) = TString
valueType (Uninitialized) = error "uninitialized value on return"
valueType (Exit v) =  valueType v


evalProg :: ScopeConfig -> [Stmt] ->Either EvalError (SymbolValue, Environment, Log)
evalProg conf block = RWS.runRWST (do
					ret<-evalBlock (Block 0 0 block) "Global"
					addLog EndProg
					return ret) conf newEnvironment

eval :: Stmt-> RWS.RWST ScopeConfig Log Environment (Either EvalError) SymbolValue
eval inst@(CallPrint _ e) = do
				addLog inst
				evalType e
				evalExpr e >>= printVal
eval inst@(CallPrintLn _ e) = do
				addLog inst
				evalType e
				evalExpr e >>= printValLn
eval inst@(DeclVar lineno name typ) =do
				addLog inst
				insertSymbol lineno False name (typ,Uninitialized) 
				return Void
eval inst@(DeclFunct lineno name arg typ block) = do
				addLog inst
				getCurrentStack >>= 
						(\x -> insertSymbol lineno False name (TFunct, ValFun{
																valFunargs=arg,
																valFunType=typ,
																valFunBlock=block,
																valFunParent=x,
																valFunEnv=Nothing
															})) 
				return Void
eval inst@(Assign lineno name e) = do
				addLog inst
				(t1,_)<-lookupSymbol lineno name
				t2<- evalType e 
				if t1/=t2
					then RWS.lift $ Left $ WrongType lineno t1 t2
					else do 
						x <- (,)t2<$> evalExpr e 
						assignSymbol name x
						return Void 
eval inst@(CallProc lineno name arg) = do
				addLog inst
				evalType (CallFunct lineno name arg)
				lookupSymbol lineno name >>= (\x -> case x of
					(TFunct,ValFun{
								valFunargs=fargs,
								valFunBlock=block@(Block _lineno' n _),
								valFunParent=parent,
								valFunEnv=env,
								valFunType=typ
								}) -> do 
									mapM evalExpr arg >>= insertArgs n parent fargs env
									ret <- typecheck typ =<< evalBlock block name
									exitScope
									return ret

					(t,_) -> RWS.lift $ Left $ WrongType lineno t TFunct 
							)	
						where
							typecheck typ val =if typ == valueType val 
												then case val of
													Exit v -> return v
													Void -> return Void
													_ -> error "unexpected return from block evaluation"
												else
													RWS.lift $ Left $ WrongType lineno (valueType val) typ
							insertArgs n parent fargs env args' = do 
								newScope ("args.of."++name++"|"++show n) parent env
								zipWithM (\(nam,t) v-> insertSymbol lineno True  nam (t,v)) fargs args' 
								
eval inst@(ControlIf lineno cond tblock fblock) = do
								addLog inst
								evalType cond >>= (\x -> unless (x == TBool) (RWS.lift $ Left $ WrongType lineno x TBool))
								(ValBool x) <- evalExpr cond
								if x then evalBlock tblock "ifTbranch" else evalBlock fblock "ifFbranch"

eval inst@(Return _ a) = addLog inst >>liftM Exit (maybe (return Void)  evalExpr a)
eval (EndProg) = error "EndProg is a dummy value"

evalBlock :: Block->  String -> RWS.RWST ScopeConfig Log Environment (Either EvalError) SymbolValue
evalBlock (Block _ n b) name = do
						getCurrentStack >>= flip (newScope (name++"|"++show n)) Nothing
						when (flagDebug) $ flip trace (return ()) . show =<< RWS.get
						retVal <-eval' b
						exitScope
						return retVal
		where
			eval' [] = return Void
			eval' (x:xs) = do
						ret <- eval x 
						case ret of
							Void -> eval' xs
							a@(Exit val) -> return a
							_ -> eval' xs
									


evalType :: Expr -> RWS.RWST ScopeConfig Log Environment (Either EvalError) Type
evalType (IntLiteral{}) = return TInt
evalType (BoolLiteral{}) = return TBool
evalType (StringLiteral{}) = return TString
evalType (Var lineno n) = RWS.asks lookupScope >>= 
					(\f -> f n) >>= 
					maybe (RWS.lift $ Left $ NotInScope lineno n) (return.fst)
evalType (UnaryExp lineno op e) = evalType e >>= 
							(\x -> if x==t'
									then return x
									else RWS.lift $ Left $ WrongType lineno x t')
					where
						t' = case op of
								Negate -> TInt
								Not -> TBool
evalType (BinaryExp lineno op a b) = do 
					evalType a >>= check
					evalType b >>= check 
					where
						check e = if e==opT then return resT else RWS.lift $ Left $ WrongType lineno e opT
							where
								(opT, resT) = case op of 
									Add -> (TInt , TInt)
									Minus -> (TInt  , TInt)
									Mult -> (TInt , TInt)
									Div -> (TInt , TInt)
									Mod -> (TInt , TInt)
									Or -> (TBool ,TBool)
									And -> (TBool ,TBool)
									LessTE -> (TInt ,TBool)
									LessT -> (TInt ,TBool)
									GreaterTE -> (TInt ,TBool)
									GreaterT ->( TInt ,TBool)
									Equals -> (e ,TBool)
									NEquals ->( e ,TBool)
evalType (CallFunct lineno n callargs) = RWS.asks lookupScope >>=
								 (\f -> f n) >>=
								 maybe (RWS.lift $ Left $ NotInScope lineno n) go
					where
						check (_,typ) ex = evalType ex >>= 
							(\x -> if x==typ 
									then return typ
									else RWS.lift $ Left $ WrongType lineno  x typ)
						go (TFunct,ValFun{valFunargs=declargs,valFunType=t}) = do
								when (callen /= declen) (RWS.lift $ Left $ LessArgs lineno n callen declen)
								zipWithM_ check declargs callargs 
								return t
									where
										callen = length callargs
										declen = length declargs
						go (t,_) = RWS.lift $ Left $ WrongType lineno t TFunct

evalExpr :: Expr -> RWS.RWST ScopeConfig Log Environment (Either EvalError) SymbolValue
evalExpr (IntLiteral _ n) = return $ ValInt n
evalExpr (BoolLiteral _ b) = return $ ValBool b
evalExpr (StringLiteral _ s) = return $ ValString s
evalExpr (Var lineno n) = do
					x <- RWS.asks lookupScope >>= (\f -> f n)
					case fromJust x of
						(_,Uninitialized) -> RWS.lift $ Left $ VariableNotInitialized lineno n
						(_,m) -> return m
evalExpr (UnaryExp _lineno op e) = evalExpr e >>= f' op
				where
					f' Negate (ValInt n) = return $ValInt (-n)
					f' Not (ValBool b) = return $ValBool (not b)
					f' _ _  = error "unsupported unary expression"
evalExpr (BinaryExp _lineno op aexp bexp) = f' op <$> evalExpr aexp <*> evalExpr bexp
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
evalExpr (CallFunct lineno n cargs) = eval (CallProc lineno n cargs)


execString ::  ScopeConfig -> String -> Either ParseError (Either EvalError (SymbolValue, Environment, Log))
execString mode = liftM (evalProg mode) . runParser prog 0 "Input"

exec :: ScopeConfig -> SourceName -> IO (Either ParseError (Either EvalError (SymbolValue, Environment, Log)))
{-exec mode file = return . liftM (evalProg mode) . runParser prog 0 file  =<< readFile file-}
exec mode file = return . execString mode =<< readFile file
