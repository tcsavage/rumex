{-# LANGUAGE ScopedTypeVariables #-}

module Compiler (compile) where

import Prelude hiding (Left, Right)
import Tokens (tokenize)
import Parser

import Control.Monad
import Data.Maybe
import Data.Word
import LLVM.Core
import LLVM.Util.Loop (mapVector)
import Data.TypeLevel.Num (D8, toNum)
import System.Process (system)
import Text.Printf

--getTapeSize :: Vector s Word8 -> Word32
--getTapeSize = toNum (undefined :: s)

data GlobalRefs = GlobalRefs {tapeVec :: Global (Vector D8 Word8), tapeHead :: Global Word32, pint :: Function (Word8 -> IO Word32)}

--tapeVec :: TGlobal (Vector D8 Word8)
--tapeVec = createNamedGlobal False ExternalLinkage "tape" $ constVector (map constOf [1..8::Word8])

--tapeHead :: TGlobal Word32
--tapeHead = createNamedGlobal False ExternalLinkage "tapehead" $ constOf (0::Word32)

getCurrent :: GlobalRefs -> TFunction (IO Word8)
getCurrent refs = do
	let h = tapeHead refs
	let tape = tapeVec refs
	f <- newNamedFunction ExternalLinkage "getcurrent" :: TFunction (IO Word8)
	defineFunction f $ do
		pos <- load h
		ctape <- load tape
		curr <- extractelement ctape pos
		ret curr
	return f

moveRight :: GlobalRefs -> TFunction (Word32 -> IO ())
moveRight refs = do
	let h = tapeHead refs
	f <- newNamedFunction ExternalLinkage "moveRight" :: TFunction (Word32 -> IO ())
	defineFunction f $ \n -> do
		old <- load h
		new <- add n old
		store new h
		ret ()
	return f

moveLeft :: GlobalRefs -> TFunction (Word32 -> IO ())
moveLeft refs = do
	let h = tapeHead refs
	f <- newNamedFunction ExternalLinkage "moveLeft" :: TFunction (Word32 -> IO ())
	defineFunction f $ \n -> do
		old <- load h
		new <- sub old n
		store new h
		ret ()
	return f

incrementCurrent :: GlobalRefs -> TFunction (Word32 -> IO ())
incrementCurrent refs = do
	let h = tapeHead refs
	let tape = tapeVec refs
	f <- newNamedFunction ExternalLinkage "inc" :: TFunction (Word32 -> IO ())
	defineFunction f $ \n -> do
		pos <- load h
		tape' <- load tape
		old <- extractelement tape' pos
		(old32 :: Value Word32) <- zext old -- Word8 -> Word32
		new <- add old32 n
		new8 <- trunc new -- Word32 -> Word8
		tape'' <- insertelement tape' new8 pos
		store tape'' tape
		ret ()
	return f

decrementCurrent :: GlobalRefs -> TFunction (Word32 -> IO ())
decrementCurrent refs = do
	let h = tapeHead refs
	let tape = tapeVec refs
	f <- newNamedFunction ExternalLinkage "dec" :: TFunction (Word32 -> IO ())
	defineFunction f $ \n -> do
		pos <- load h
		tape' <- load tape
		old <- extractelement tape' pos
		(old32 :: Value Word32) <- zext old -- Word8 -> Word32
		new <- sub old32 n
		new8 <- trunc new -- Word32 -> Word8
		tape'' <- insertelement tape' new8 pos
		store tape'' tape
		ret ()
	return f

printCurrentAscii :: GlobalRefs -> TFunction (Word32 -> IO ())
printCurrentAscii refs = do
	let h = tapeHead refs
	let tape = tapeVec refs
	putchar <- newNamedFunction ExternalLinkage "putchar" :: TFunction (Word32 -> IO Word32)
	f <- newNamedFunction ExternalLinkage "pascii" :: TFunction (Word32 -> IO ())
	defineFunction f $ \n -> do
		pos <- load h
		tape' <- load tape
		current <- extractelement tape' pos
		(current32 :: Value Word32) <- zext current -- Word8 -> Word32
		void $ call putchar current32
		ret ()
	return f

printCurrentInt :: GlobalRefs -> TFunction (Word32 -> IO ())
printCurrentInt refs = do
	let h = tapeHead refs
	let tape = tapeVec refs
	let pint' = pint refs
	f <- newNamedFunction ExternalLinkage "pcint" :: TFunction (Word32 -> IO ())
	defineFunction f $ \n -> do
		pos <- load h
		tape' <- load tape
		current <- extractelement tape' pos
		void $ call pint' current
		ret ()
	return f

putint :: TFunction (Word8 -> IO Word32)
putint = withStringNul "%d\n" $ \t -> do
	f <- newNamedFunction ExternalLinkage "pint" :: TFunction (Word8 -> IO Word32)
	printf <- newNamedFunction ExternalLinkage "printf" :: TFunction (Ptr Word8 -> Word8 -> IO Word32)
	defineFunction f $ \x -> do
		tmp <- getElementPtr0 t (0::Word32, ())
		r <- call printf tmp x
		ret r
	return f

printTape :: GlobalRefs -> TFunction (IO ())
printTape refs = do
	let tape = tapeVec refs
	let pint' = pint refs
	f <- newNamedFunction ExternalLinkage "printtape" :: TFunction (IO ())
	defineFunction f $ do
		tape' <- load tape
		mapVector (\x -> call pint' x) tape'
		ret ()
	return f

build :: [(Op, Function (Word32 -> IO ()))] -> Prog -> CodeGenFunction r ()
build funTable (Join a b) = (build funTable a) >> (build funTable b)
build funTable (CommandSequence (Command n op)) = void $ call (fromJust $ lookup op funTable) (valueOf $ toEnum n)
build _ (Loop prog) = undefined
build _ (FunDef prog) = undefined
build _ (Str str) = undefined

llvmModule :: Prog -> TFunction (IO Word32)
llvmModule ast = do
	-- Generate globals and fun refs
	vec <- createNamedGlobal False ExternalLinkage "tape" $ constVector (map constOf (take 8 (repeat (0::Word8))))
	head <- createNamedGlobal False ExternalLinkage "tapehead" $ constOf (0::Word32)
	pint <- putint
	let grefs = GlobalRefs vec head pint
	-- Build functions
	dbgprint <- printTape grefs
	getval <- getCurrent grefs
	r <- moveRight grefs
	l <- moveLeft grefs
	inc <- incrementCurrent grefs
	dec <- decrementCurrent grefs
	pascii <- printCurrentAscii grefs
	pcint <- printCurrentInt grefs
	-- Function table
	let funTable = [(Inc, inc), (Dec, dec), (Left, l), (Right, r), (WriteChar, pascii), (WriteInt, pcint)]
	-- Define main
	main <- newNamedFunction ExternalLinkage "main" :: TFunction (IO Word32)
	defineFunction main $ do
		build funTable ast
		--void $ call dbgprint
		ret (0::Word32)
	return main

compile :: String -> FilePath -> IO ()
compile progcode out = do
	let ast = parse $ tokenize progcode
	m <- newNamedModule out
	prog <- defineModule m (llvmModule ast)
	dumpValue prog
	writeBitcodeToFile (out++".bc") m
	void $ system $ printf "llvm-ld -o %s -native %s.bc" out out
