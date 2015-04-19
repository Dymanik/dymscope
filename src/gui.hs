import Graphics.UI.Gtk
import Graphics.UI.Gtk.SourceView
import Data.List(sortBy,intersperse)
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

stackWidth ::  Double
stackWidth  = 150

stackHeigth ::  Double
stackHeigth  = 20


drawClosure :: M.Map String (Integer,Int) -> Render ()
drawClosure vals = do
			save
			translate stackWidth 0
			moveTo 0 10
			lineTo 20 10
			stroke
			translate 20 0
			F.foldlM  go 0 $ M.toAscList vals
			restore
				where
					go n (k,(num,v)) = do 
						Cairo.rectangle 0 0 60 stackHeigth
						(\ (r,g,b) -> setSourceRGB r g b) $ chooseColor (fromInteger num)
						fill
						setSourceRGB 0 0 0
						moveTo 2 15
						showText (k ++"=>"++show v)
						Cairo.rectangle 0 0 60 stackHeigth
						stroke
						translate 60 0
						return (n+1)

stackUnit :: String -> [SymbolValue] -> Int -> Int -> Bool -> Render ()
stackUnit _ [] _ _ _ = error "empty Value"
stackUnit name (value:oldVal) pos n isDeep = do
		Cairo.rectangle 0 0 stackWidth stackHeigth
		(\(r,g,b) -> setSourceRGB r g b) $ chooseColor n
		fill
		setSourceRGB 0 0 0
		moveTo 2 15
		showText (show pos)
		moveTo 20 0
		lineTo 20 stackHeigth
		moveTo 22 15
		showText (varText)
		(x',y') <- getCurrentPoint
		showText (oldValues)
		relMoveTo 0 (-3)
		lineTo x' (y'-3)
		Cairo.rectangle 0 0 stackWidth stackHeigth
		stroke
		case value of
			ValFun _ _ _ _ (Just m) -> when isDeep $ drawClosure m
			_ -> return ()
			where
				varText = name ++ " = " ++ show value ++ " "
				oldValues = case filter (/= Uninitialized) oldVal of
								[] -> ""
								olds -> concat $ (",":) $ intersperse "," $ map show olds


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
			|  DrawStackUnit String [SymbolValue] Int



staticChainDraw :: M.Map String Integer ->  String -> String -> Render ()
staticChainDraw _ _ "" = return ()
staticChainDraw _ "" _ = return ()
staticChainDraw headers a b = do
		arc 0 (posY a -10) 10 (pi/2) pi
		arc 0 (posY b +10) 10 pi (3*pi/2)
		stroke
		where
			posY header = stackHeigth * fromInteger (fromJust $M.lookup header headers)  




myDraw ::  Maybe Logunit -> Render ()
myDraw Nothing = do
		setSourceRGB 1 1 1
		paint

myDraw (Just logUnit) = do
		setSourceRGB 1 1 1
		paint

		setSourceRGB 0 0 0
		save
		translate 30 10
		(_,_,headers) <- F.foldlM stackDraw (0,0,M.empty) (S.viewl stackSeq)   
		restore
		save
		translate 30 20
		when isStatic $ foldM_ (\a b -> staticChainDraw headers a b >> return b) "" (map symtableName chain)
		restore
			where
				f  = S.fromList . map go . sortBy (\a b ->compare (fst $ snd a) (fst $snd b)). M.toList
				go (k,(n,(t,v))) = DrawStackUnit (k ++ ":" ++ show t) v n
				stackSeq = F.foldMap (\x -> (DrawHeader $ symtableName x) S.<| f (symtableValues x) ) $tail $ reverse  $  envStack $ logEnv  logUnit
				stackDraw (n,h,headers) (DrawHeader s) = stackHeader s h  >> translate 0 stackHeigth >> return (n+1,h+1,M.insert s n headers)
				stackDraw (n,h,headers) (DrawStackUnit name val pos) = stackUnit name val pos (h-1) isDeep >> translate 0 stackHeigth >> return (n+1,h,headers)
				chain = init $ staticChain $ envStack $ logEnv logUnit
				isStatic = logStatic logUnit
				isDeep = logDeep logUnit


{-drawStack ::  Builder -> IORef (t, Maybe Logunit, t1) -> IO ()-}
drawStack :: TextBufferClass self =>Builder -> IORef (t, Maybe Logunit, t1) -> self -> Render ()
drawStack builder logRef codeBuffer = do
			drawArea <- liftIO $  builderGetObject builder castToDrawingArea "stackDrawArea"
			
			currLineBuffer <-  liftIO $ builderGetObject builder castToTextBuffer "currLineBuffer"
			(_,logUnit,_) <-  liftIO $ readIORef logRef
			maybe (return ()) ( liftIO . textBufferSetText currLineBuffer . show . logInst)  logUnit
			maybe (return ()) (liftIO. textBufferPlaceCursor codeBuffer <=< liftIO . textBufferGetIterAtLine codeBuffer  <=< return . (flip (-) 1) . wGetLine . logInst) logUnit 
			myDraw logUnit
			liftIO $ widgetSetSizeRequest drawArea (-1) (vars logUnit * floor stackHeigth + 50)
			where				
				vars Nothing = -1
				vars (Just logUnit) = F.foldl' (\acc x -> (acc+1) + M.size (symtableValues x))  0 $ envStack $logEnv logUnit

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

redrawStack ::  Builder -> IO ()
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

toEnd :: Builder -> IORef (Log, Maybe Logunit, Log) -> IO Bool
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
processExec builder logRef (_,_,logUnit)  = do
			outputBuffer <- builderGetObject builder castToTextBuffer "outputBuffer"
			textBufferSetText outputBuffer ""
			let (l,ls) = case S.viewl logUnit of
				(x S.:< xs) ->  (Just x,xs)
				(S.EmptyL) -> error "no output from program"
			writeIORef logRef (S.empty,l,ls)
			setSensitive builder False (not $S.null ls)

{-runButtonAction :: Builder-> IORef (Log, Maybe  Logunit, Log) -> IO Bool-}
runButtonAction :: SourceBufferClass self =>Builder-> IORef (S.Seq a, Maybe Logunit, S.Seq Logunit) -> self -> IO Bool
runButtonAction builder logRef codeBuffer = do
			tempdir <- getTemporaryDirectory
			(tempfile,tmph) <- openTempFile tempdir "run"
			hDuplicateTo tmph stdout	
			setSensitive builder False False
			redrawStack builder
			outputBuffer <- builderGetObject builder castToTextBuffer "outputBuffer"
			start <- textBufferGetStartIter codeBuffer
			end <- textBufferGetEndIter codeBuffer
			code <- textBufferGetText codeBuffer start end False
			mode <- getMode builder
			trace code $ return ()
			writeIORef logRef (S.empty,Nothing,S.empty)
			finally (do 
				unless (null code) $ either print (either print (processExec builder logRef)) $ execString mode code
				hFlush stdout
				hSeek tmph AbsoluteSeek 0
				{-sourceBufferCreateSourceMark codeBuffer Nothing "" =<< textBufferGetIterAtLine codeBuffer 0-}
				textBufferSetText outputBuffer =<< hGetContents tmph
				) (do
					hClose tmph
					removeFile tempfile
					)
			return False


sourceViewWidg ::  Builder -> SourceBuffer -> IO ()
sourceViewWidg builder buffer= do 
		sourceView <- sourceViewNewWithBuffer buffer
		scroll <- builderGetObject builder castToScrolledWindow "scrolledCode"
		sourceViewSetShowLineNumbers sourceView True
		{-sourceViewSetShowLineMarks sourceView True-}
		containerAdd scroll sourceView 

		

main ::  IO ()
main = do 
		initGUI
		builder <- builderNew
		builderAddFromFile builder  "resources/dymgtk2.glade"
		codeBuffer <- sourceBufferNew Nothing
		sourceViewWidg builder codeBuffer
		window <- builderGetObject builder castToWindow "window1"
		window `on` deleteEvent $ liftIO mainQuit >> return False
		runButton <- builderGetObject builder castToButton "runButton"
		beginningButton <- builderGetObject builder castToButton "beginningButton"
		backButton <- builderGetObject builder castToButton "backButton"
		nextButton <- builderGetObject builder castToButton "nextButton"
		endButton <- builderGetObject builder castToButton "endButton"
		logRef <- newIORef (S.empty,Nothing,S.empty)
		runButton `on` buttonReleaseEvent $ liftIO $ runButtonAction builder logRef codeBuffer
		beginningButton `on` buttonReleaseEvent $ liftIO $ toBeg builder logRef
		backButton `on` buttonReleaseEvent $ liftIO $ toBack builder logRef
		nextButton `on` buttonReleaseEvent $ liftIO $ toNext builder logRef
		endButton `on` buttonReleaseEvent $ liftIO $ toEnd builder logRef
		setSensitive builder False False 
		widgetShowAll window
		drawArea <- builderGetObject builder castToDrawingArea "stackDrawArea"
		drawArea `on` draw $ drawStack builder logRef codeBuffer
		mainGUI
