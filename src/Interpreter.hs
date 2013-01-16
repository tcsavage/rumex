module Interpreter (interpret) where

import Prelude hiding (Left, Right)
import Tokens (tokenize)
import Parser

import Control.Monad.Trans.State.Lazy
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import Data.Vector ((!))
import Data.Word
import Text.Printf

type InterpreterState = State (DataTape, String, FunTable, String) String

interpret :: String -> String -> String
interpret inp progcode = evalState (runProg . parse . tokenize $ progcode) (initState inp)

interpretDebug :: String -> String -> String
interpretDebug inp progcode = printf "Output: %s\n\nFinal Tape:\n%s\n\nUnconsumed Input: %s" out (drawTape 12 dt) inp
	where
		(out, (dt, inp, _, _)) = runState (runProg . parse . tokenize $ progcode) (initState inp)

runProg :: Prog -> InterpreterState
runProg (Join p1 p2) = (runProg p1) >> (runProg p2)
runProg (CommandSequence c) = runCommand c
runProg (Loop l) = do
	(dt, inp, ft, out) <- get
	case focus dt of
		0 -> return out
		otherwise -> runProg l >> (runProg $ Loop l)
runProg (FunDef f) = do
	(dt, inp, ft, out) <- get
	let ft' = V.imap (\i v -> if i == (fromEnum $ focus dt) then Just f else v) ft
	put (dt, inp, ft', out)
	return out
runProg (Str s) = do
	(dt, inp, ft, out) <- get
	put (dt, inp ++ s ++ "\0", ft, out)
	return out

runCommand :: Command -> InterpreterState
runCommand (Command n op) = do
	out <- sequence . replicate n $ runOp op
	return $ last out

runOp :: Op -> InterpreterState
runOp Invoke = get >>= (\(dt, inp, ft, out) -> runProg $ fromJust $ ft ! (fromEnum $ focus dt))
runOp op = do
	(dt, inp, ft, out) <- get
	case op of
		Inc -> put (modFocus (+1) dt, inp, ft, out)
		Dec -> put (modFocus (\x -> x-1) dt, inp, ft, out)
		Left -> put (l dt, inp, ft, out)
		Right -> put (r dt, inp, ft, out)
		ReadChar -> doReadChar (dt, inp, ft, out)
		WriteChar -> put (dt, inp, ft, out ++ [writeChar $ focus dt])
		WriteInt -> put (dt, inp, ft, out ++ (writeInt $ focus dt))
	(_, _, _, out') <- get
	return out'
	where
		doReadChar (dt, [], ft, out) = return ()
		doReadChar (dt, (i:is), ft, out) = put (modFocus (const $ readChar i) dt, is, ft, out)

type FunTable = V.Vector (Maybe Prog)

initFunTable :: FunTable
initFunTable = V.replicate 256 Nothing

data Tape a = Tape [a] a [a] deriving (Eq)

focus :: Tape a -> a
focus (Tape _ f _) = f

modFocus :: (a -> a) -> Tape a -> Tape a
modFocus f (Tape a b c) = Tape a (f b) c

l :: Tape a -> Tape a
l (Tape [] _ _) = error "Reached end of tape"
l (Tape (a:as) b c) = Tape as a (b:c)

r :: Tape a -> Tape a
r (Tape _ _ []) = error "Reached end of tape"
r (Tape a b (c:cs)) = Tape (b:a) c cs

initTape :: [a] -> Tape a
initTape (x:xs) = Tape [] x xs

instance Functor Tape where
	fmap f (Tape a b c) = Tape (map f a) (f b) (map f c)

drawTape :: Show a => Int -> Tape a -> String
drawTape n (Tape r v f) = printf "... %s  [%s]  %s ..." (drawList $ reverse $ take n r) (drawCell v) (drawList $ take n f)
	where
		drawList = concat . (intersperse "  ") . (map drawCell)
		drawCell = show

type DataTape = Tape Word8

initDataTape :: DataTape
initDataTape = initTape $ repeat 0

readChar :: Char -> Word8
readChar = toEnum . fromEnum

writeChar :: Word8 -> Char
writeChar = toEnum . fromEnum

writeInt :: Word8 -> String
writeInt = (filter isDigit) . show

initState :: String -> (DataTape, String, FunTable, String)
initState inp = (initDataTape, inp, initFunTable, "")
