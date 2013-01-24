module Compiler (compile) where

import Prelude hiding (Left, Right)
import Tokens (tokenize)
import Parser

import Control.Monad
import Data.Word
import LLVM.Core
import Data.TypeLevel.Num (D8, toNum)
import System.Process (system)
import Text.Printf

--getTapeSize :: Vector s Word8 -> Word32
--getTapeSize = toNum (undefined :: s)

data GlobalRefs = GlobalRefs {tapeVec :: Global (Vector D8 Word8), tapeHead :: Global Word32}

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
		new <- add old n
		tape'' <- insertelement tape' new pos
		store tape'' tape
		ret ()
	return f

pint :: TFunction (Word8 -> IO Word32)
pint = withStringNul "%d\n" $ \t -> do
	f <- newNamedFunction ExternalLinkage "pint" :: TFunction (Word8 -> IO Word32)
	printf <- newNamedFunction ExternalLinkage "printf" :: TFunction (Ptr Word8 -> Word8 -> IO Word32)
	defineFunction f $ \x -> do
		tmp <- getElementPtr0 t (0::Word32, ())
		r <- call printf tmp x
		ret r
	return f

llvmModule :: TFunction (IO Word32)
llvmModule = do
	vec <- createNamedGlobal False ExternalLinkage "tape" $ constVector (map constOf [1..8::Word8])
	head <- createNamedGlobal False ExternalLinkage "tapehead" $ constOf (0::Word32)
	let grefs = GlobalRefs vec head
	putchar <- newNamedFunction ExternalLinkage "putchar" :: TFunction (Word32 -> IO Word32)
	putint <- pint
	getval <- getCurrent grefs
	r <- moveRight grefs
	l <- moveLeft grefs
	inc <- incrementCurrent grefs
	let h = tapeHead grefs
	let tape = tapeVec grefs
	main <- newNamedFunction ExternalLinkage "main" :: TFunction (IO Word32)
	defineFunction main $ do
		cpos <- load h
		ctape <- load tape
		ctape' <- insertelement ctape (valueOf 99) (valueOf 2)
		store ctape' tape
		ctape'' <- load tape
		celem <- extractelement ctape'' (valueOf 2)
		_ <- call putint celem
		_ <- call inc (valueOf 2)
		_ <- call r (valueOf 4)
		currval <- call getval
		_ <- call putint currval
		ret (0::Word32)
	return main

compile :: String -> FilePath -> IO ()
compile progcode out = do
	let ast = parse $ tokenize progcode
	m <- newNamedModule out
	prog <- defineModule m llvmModule
	dumpValue prog
	writeBitcodeToFile (out++".bc") m
	void $ system $ printf "llvm-ld -o %s -native %s.bc" out out
