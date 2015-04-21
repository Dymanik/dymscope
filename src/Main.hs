module Main (main) where
import Parser
import System.Environment
import System.Console.GetOpt
import Control.DeepSeq



options  :: [OptDescr ScopeConfig]
options = 
	[ Option [] ["dynamic-deep","dd"] (NoArg dynDeepScope) "Use Dynamic Scoping with Deep Binding",
	  Option [] ["static-deep","sd"] (NoArg staticDeepScope) "Use Static Scoping with Deep Binding",
	  Option [] ["static-shallow","ss"] (NoArg staticShallowScope) "Use Static Scoping with Shallow Binding(Default)",
	  Option [] ["dynamic-shallow","ds"] (NoArg dynShallowScope) "Use Static Scoping with Shallow Binding"
	]

compilerOpts :: [String] -> IO (ScopeConfig, [String])
compilerOpts argv = case getOpt RequireOrder options argv of
						(_,[],_) -> ioError (userError ("missing arguments\n" ++ usageInfo header options))
						([],n,[]  ) -> return (staticShallowScope, n)
						(o:_ , n , []) -> return (o, n)
						(_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
					where header = "Usage: dymScope <OPTIONS> <file>"

main ::  IO ()
main = do
		(config,l) <- compilerOpts =<< getArgs
		exec  config (head l)  >>= either print (either print (\(x,_,_) -> x `deepseq` return () ) )
