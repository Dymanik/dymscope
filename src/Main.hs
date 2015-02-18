
import Parser

import System.Environment
import System.Console.GetOpt
import Control.DeepSeq
import Data.Either


defaultConfig = ScopeConfig {
					staticScoping=True,
					deepBinding=False,
					lookupScope=lookupStatic,
					assignScope=assignStatic
				}

options  :: [OptDescr (ScopeConfig -> ScopeConfig)]
options = 
	[ Option ['D'] ["dynamic"] (NoArg (\opts -> opts {staticScoping=False,lookupScope=lookupDyn,assignScope=assignDyn})) "Use Dynamic Scoping",
	  Option ['S'] ["static"] (NoArg (\opts -> opts {staticScoping=True,lookupScope=lookupStatic,assignScope=assignStatic})) "Use Static Scoping",
	  Option ['d'] ["deep"] (NoArg (\opts -> opts {deepBinding=True})) "Use Deep Binding",
	  Option ['s'] ["shallow"] (NoArg (\opts -> opts {deepBinding=False})) "Use Shallow Binding"
	]

compilerOpts :: [String] -> IO (ScopeConfig, [String])
compilerOpts argv = case getOpt RequireOrder options argv of
						(o,n,[]  ) -> return (foldl (flip id) defaultConfig o, n)
						(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
					where header = "Usage: main <OPTIONS> <file>"


main = do
		(config,l) <- compilerOpts =<< getArgs
		(exec (head l) config )  >>= either (putStrLn.show) (\(x,y,z) -> x `deepseq` (return ())) 
		{-(exec (head l) config )  >>= either (putStrLn.show) (putStrLn.show) -}
