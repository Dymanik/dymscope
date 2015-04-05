import Graphics.UI.Gtk
import Control.Monad.IO.Class
import Parser
import Data.IORef
import Control.Monad
import qualified Data.Sequence as S
import Graphics.Rendering.Cairo as Cairo
import Control.Applicative
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Foldable as F
import Debug.Trace
import System.Directory
import GHC.IO.Handle
import System.IO 
import Control.Exception(finally)

getMode ::  Builder -> IO ScopeConfig
getMode builder = do
			isStatic <- toggleButtonGetActive =<< builderGetObject builder castToRadioButton "staticButton"
			isShallow <- toggleButtonGetActive =<< builderGetObject builder castToRadioButton "shallowButton"
			return $ case (isStatic,isShallow) of
					(True,True) -> staticShallowScope
					(False,True) -> dynShallowScope
					(True,False) ->  staticDeepScope
					(False,False) -> dynDeepScope

--CONSTANTS

colorlist ::  [(Double, Double, Double)]
colorlist = [color1,color2,color3]
color1 ::  (Double, Double, Double)
color1 = (166/255, 221/255, 99/255) 
color2 ::  (Double, Double, Double)
color2 = (255/255, 107/255, 107/255) 
color3 ::  (Double, Double, Double)
color3 = (136/255, 186/255, 194/255) 

chooseColor ::  Int -> (Double, Double, Double)
chooseColor n = colorlist !! (n `mod` length colorlist)

stackWidth  = 150
stackHeigth  = 20


drawClosure :: M.Map String Integer -> Render ()
drawClosure vals = do
			save
			translate stackWidth 0
			moveTo 0 10
			lineTo 20 10
			stroke
			translate 15 0
			F.foldlM  go 0 $ M.toAscList vals
			restore
				where
					go n (k,v) = do 
						Cairo.rectangle 0 0 60 stackHeigth
						(\ (r,g,b) -> setSourceRGB r g b) $ chooseColor n
						fill
						setSourceRGB 0 0 0
						moveTo 0 15
						showText (k ++"=>"++show v)
						Cairo.rectangle 0 0 60 stackHeigth
						stroke
						translate 60 0
						return (n+1)




						

stackUnit :: String -> SymbolValue -> Integer -> Int -> Render ()
stackUnit name value pos n = do
		Cairo.rectangle 0 0 stackWidth stackHeigth
		(\(r,g,b) -> setSourceRGB r g b) $ chooseColor n
		fill
		setSourceRGB 0 0 0
		moveTo 2 15
		showText (show pos)
		moveTo 20 0
		lineTo 20 stackHeigth
		moveTo 22 15
		showText (name ++ " = " ++ show value)
		Cairo.rectangle 0 0 stackWidth stackHeigth
		stroke
		case value of
			ValFun _ _ _ _ (Just m) -> drawClosure m
			_ -> return ()


{-stackHeader :: String -> Integer -> Int ->  Render ()-}
stackHeader ::  String -> Int -> Render ()
stackHeader name hn  = do
		Cairo.rectangle 0 0 stackWidth stackHeigth
		(\(r,g,b) -> setSourceRGB r g b) $ chooseColor hn
		fill
		moveTo 2 15
		setSourceRGB 0 0 0
		showText name
		Cairo.rectangle 0 0 stackWidth stackHeigth
		stroke


data LogDraw = DrawHeader String
			|  DrawStackUnit String SymbolValue



staticChainDraw :: M.Map String Integer ->  String -> String -> Render ()
staticChainDraw _ _ "" = return ()
staticChainDraw _ "" _ = return ()
staticChainDraw headers a b = do
		arc 0 (posY a -10) 10 (pi/2) (pi)
		arc 0 (posY b +10) 10 (pi) (3*pi/2)
		stroke
		where
			posY header = (fromInteger $ fromJust $M.lookup header headers) * stackHeigth 




myDraw ::  Maybe Logunit -> Render ()
myDraw Nothing = return ()
myDraw (Just log) = do
		setSourceRGB 0 0 0
		moveTo 10 10
		showText $ show $ logInst log
		save
		translate 30 30
		(_,_,_,headers,closures) <- F.foldlM stackDraw (0,0,0,M.empty,0) (S.viewl stackSeq)   
		restore
		save
		translate 30 40
		foldM_ (\a b -> staticChainDraw headers a b >> return b) "" (map symtableName chain)
		restore
			where
				f  = S.fromList . map go . M.toAscList
				go (k,(t,v)) = DrawStackUnit (k ++ ":" ++ show t) (v)

				stackSeq = F.foldMap (\x -> (DrawHeader $ symtableName x) S.<| f (symtableValues x) ) $ reverse $  envStack $ logEnv  log
				stackDraw (n,m,h,headers,closures) (DrawHeader s) = stackHeader s h  >> translate 0 stackHeigth >> return (n+1,m,h+1,M.insert s n headers,closures)
				stackDraw (n,m,h,headers,closures) (DrawStackUnit name val) = stackUnit name val m (h-1) >> translate 0 stackHeigth >> return (n+1,m+1,h,headers,closures)
				chain = staticChain $ envStack $ logEnv log


drawStack ::  Builder -> IORef (t, Maybe Logunit, t1) -> IO ()
drawStack builder logRef = do
			drawArea <- builderGetObject builder castToDrawingArea "stackDrawArea"
			drawWindow <- fromJust <$> widgetGetWindow drawArea
			(_,log,_) <- readIORef logRef
			renderWithDrawWindow  drawWindow  (myDraw log)
			widgetSetSizeRequest drawArea (-1) (vars log * (floor stackHeigth)+50)
			where				
				vars Nothing = -1
				vars (Just log) = F.foldl' (\acc x -> (acc+1) + M.size (symtableValues x))  0 $ envStack $logEnv log

			



setSensitive ::  Builder -> Bool -> Bool -> IO ()
setSensitive builder backT nextT = do
			beginningButton <- builderGetObject builder castToButton  "beginningButton"
			widgetSetSensitive beginningButton backT
			backButton <- builderGetObject builder castToButton  "backButton"
			widgetSetSensitive backButton backT
			nextButton <- builderGetObject builder castToButton  "nextButton"
			widgetSetSensitive nextButton nextT
			endButton <- builderGetObject builder castToButton  "endButton"
			widgetSetSensitive endButton nextT

redrawStack builder = do
			drawWindow <- fromJust <$> (widgetGetWindow =<< builderGetObject builder castToDrawingArea "stackDrawArea")
			drawWindowInvalidateRect drawWindow (Rectangle 0 0 1000 1000) True
			return ()
			

toBeg ::  Builder -> IORef (Log,Maybe Logunit , Log) -> IO Bool
toBeg builder  logRef = do 
			(beg,x,end) <- readIORef logRef
			let a@(backs,x',nexts) = case S.viewl beg of
				(l S.:< ls) -> (S.empty , Just l, ls S.>< ( fromJust x S.<| end))
				(S.EmptyL) -> (S.empty,x,end)
			writeIORef logRef a
			setSensitive builder (not $S.null backs) (not $S.null nexts) 
			redrawStack builder
			trace (show x') $ return ()
			return False

toBack ::  Builder -> IORef (Log,Maybe Logunit , Log) -> IO Bool
toBack builder  logRef = do 
			(beg,x,end) <- readIORef logRef
			let a@(backs,x',nexts) = case S.viewr beg of
				(ls S.:> l) -> (ls, Just l, fromJust x S.<| end)
				(S.EmptyR) -> (S.empty,x,end)
			writeIORef logRef a
			setSensitive builder (not $S.null backs) (not $S.null nexts) 
			redrawStack builder
			trace (show x') $ return ()
			return False

toNext ::  Builder -> IORef (Log,Maybe Logunit , Log) -> IO Bool
toNext builder  logRef = do 
			(beg,x,end) <- readIORef logRef
			let a@(backs,x',nexts) = case S.viewl end of
				(l S.:< ls) -> (beg S.|> fromJust x , Just l ,ls)
				(S.EmptyL) -> (S.empty,x,end)
			writeIORef logRef a
			setSensitive builder (not $S.null backs) (not $S.null nexts) 
			redrawStack builder
			trace (show x') $ return ()
			return False

toEnd builder  logRef = do 
			(beg,x,end) <- readIORef logRef
			let a@(backs,x',nexts) = case S.viewr end of
				(ls S.:> l) -> ((beg S.|> fromJust x) S.>< ls , Just l, S.empty)
				(S.EmptyR) -> (S.empty,x,end)
			writeIORef logRef a
			setSensitive builder (not $S.null backs) (not $S.null nexts) 
			redrawStack builder
			trace (show x') $ return ()
			return False

processExec :: Builder-> IORef (S.Seq a, Maybe a1, S.Seq a1)-> (t, t1, S.Seq a1)-> IO ()
processExec builder logRef (ret,env,log)  = do
			outputBuffer <- builderGetObject builder castToTextBuffer "outputBuffer"
			textBufferSetText outputBuffer ""
			let (l,ls) = case S.viewl log of
				(x S.:< xs) ->  (Just x,xs)
			writeIORef logRef (S.empty,l,ls)
			setSensitive builder False (not $S.null ls)

runButtonAction :: Builder-> IORef (Log, Maybe  Logunit, Log) -> IO Bool
runButtonAction builder logRef = do
			tempdir <- getTemporaryDirectory
			(tempfile,tmph) <- openTempFile tempdir "run"
			hDuplicateTo tmph stdout	
			setSensitive builder False False
			redrawStack builder
			codeBuffer <-builderGetObject builder castToTextBuffer "codeBuffer"
			outputBuffer <- builderGetObject builder castToTextBuffer "outputBuffer"
			start <- textBufferGetStartIter codeBuffer
			end <- textBufferGetEndIter codeBuffer
			code <- textBufferGetText codeBuffer start end False
			mode <- getMode builder
			trace code $ return ()
			writeIORef logRef (S.empty,Nothing,S.empty)
			unless (null code) $ either (print) (either (print) (processExec builder logRef)) $ execString mode code
			hFlush stdout
			hSeek tmph AbsoluteSeek 0
 			textBufferSetText outputBuffer =<< hGetContents tmph
			hClose tmph
			removeFile tempfile
			return False


withTempFile ::  String -> (FilePath -> Handle -> IO b) -> IO b
withTempFile pattern func = do 
			tempdir <- getTemporaryDirectory
			(tempfile, temph) <- openTempFile tempdir pattern 
			
			finally (func tempfile temph) 
				(do {hClose temph;
					removeFile tempfile})


main ::  IO ()
main = do 
		initGUI
		builder <- builderNew
		builderAddFromFile builder  "dymgtk.glade"
		window <- builderGetObject builder castToWindow "window1"
		window `on` deleteEvent $ liftIO mainQuit >> return False
		runButton <- builderGetObject builder castToButton "runButton"
		beginningButton <- builderGetObject builder castToButton "beginningButton"
		backButton <- builderGetObject builder castToButton "backButton"
		nextButton <- builderGetObject builder castToButton "nextButton"
		endButton <- builderGetObject builder castToButton "endButton"
		logRef <- newIORef (S.empty,Nothing,S.empty)
		runButton `on` buttonReleaseEvent $ liftIO $ runButtonAction builder logRef
		beginningButton `on` buttonReleaseEvent $ liftIO $ toBeg builder logRef
		backButton `on` buttonReleaseEvent $ liftIO $ toBack builder logRef
		nextButton `on` buttonReleaseEvent $ liftIO $ toNext builder logRef
		endButton `on` buttonReleaseEvent $ liftIO $ toEnd builder logRef
		setSensitive builder False False 
		widgetShowAll window
		drawWindow <- builderGetObject builder castToDrawingArea "stackDrawArea"
		drawWindow `on` draw $ liftIO $ drawStack builder logRef 
		scrollWindow <- builderGetObject builder castToScrolledWindow "drawScroll"
		scrollWindow `on` scrollEvent $ liftIO $ redrawStack builder >> return False 
		mainGUI
