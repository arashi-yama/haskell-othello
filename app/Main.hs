{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <&>" #-}
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector as V
import Control.Monad.Primitive (PrimState)
import Data.Maybe (isJust, fromJust)
import Control.Monad (forM_, when)
import Data.Char (isDigit)

data Piece=Null|White|Black deriving (Eq)
data Result=Success|Fail deriving (Eq,Show)

instance Show Piece where
  show Null="/"
  show White="o"
  show Black="x"

reversePiece :: Piece -> Piece
reversePiece Null=Null
reversePiece White=Black
reversePiece Black=White

newtype Board=Board (VM.MVector (PrimState IO) Piece)

newtype Coordinate=Coordinate (Int,Int) deriving Show

newCd :: (Int, Int) -> Coordinate
newCd t
  |not $ validateCd t=error "Out of bounds"
  |otherwise=Coordinate t

--有効ならTrueを返す
validateCd :: (Int,Int) -> Bool
validateCd (x,y)= 0<=x && x<8 && 0<=y && y<8

pieceVectors :: [(Int, Int)]
pieceVectors=[(0,-1),(-1,-1),(-1,0),(-1,1),(0,1),(1,1),(1,0),(1,-1)]
main :: IO ()
main = do
  boardM<-VM.replicate 64 Null>>=return . Board
  initBoard boardM
  printBoard boardM
  game boardM Black

game :: Board -> Piece -> IO ()
game board@(Board boardR) color=do
  ptMe<-isPutable color board
  ptOp<-isPutable (reversePiece color) board
  case (ptMe,ptOp) of
    (True,_)->do
      putStrLn ((if color==Black then "Black" else "White") ++"'s turn")
      input<-getLine>>=(\l->
        if length l >2 && isDigit (head l)&&(l!!1)==' '&&isDigit (l!!2) then do
          let x=read [head l]::Int
          let y=read [l!!2]::Int
          if validateCd (x,y) then return $ Just (x,y)
          else return Nothing
        else return Nothing)
      putChar '\n'

      if isJust input then do
        let (x,y)=fromJust input
        result<-put (newCd (x,y)) board color
        if result==Success then printBoard board>>game board (reversePiece color) else
          putStrLn "Invalid">>game board color
      else putStrLn "Invalid">>game board color

    (False,True)->do
      putStrLn ((if color==Black then "Black" else "White") ++"pass")
      game board (reversePiece color)

    (False,False)->do
      (black,white)<-V.freeze boardR>>=return . V.foldr (\a (b,w) ->if a == Black then (b+1,w) else if a==White then (b,w+1) else (b,w)) (0::Int,0::Int)
      putStrLn ("Black:"++show black)
      putStrLn ("White:"++show white++"\n")
      putStrLn $ if black==white then "Draw" else if black>white then "Black win"else "White win"


isAllZeroIOList::[IO Int]->IO Bool
isAllZeroIOList l=foldr (\a b->do
  a'<-a
  b'<-b
  return $ a'+b') (return 0) l >>=(return . (==0))

--取得するだけ
get :: Coordinate -> Board -> IO Piece
get (Coordinate (x,y)) (Board board)=VM.read board (x+y*8)

--置くだけ
set ::Coordinate-> Board -> Piece -> IO ()
set (Coordinate (x,y)) (Board board)=VM.write board (x+y*8)

--置いてひっくり返す
put :: Coordinate -> Board -> Piece -> IO Result
put cd@(Coordinate (x,y)) board piece=do
  ls<-getReversibleLengths cd board piece
  if isJust ls then do
    b<-isAllZeroIOList (fromJust ls)
    if b then return Fail
    else do
      let a=zip pieceVectors (fromJust ls)
      forM_ a $ \((dx,dy),il)->do
        l<-il
        forM_ [0..l] (\n->set (newCd (x+dx*n,y+dy*n)) board piece)
      return Success
  else return Fail

--8方向の仮にそこに置いたときにひっくり返せるコマの数
getReversibleLengths :: Coordinate -> Board -> Piece -> IO (Maybe [IO Int])
getReversibleLengths cd board color=do
  p<-get cd board
  case p of
    Null->return $ Just $ map (\v->getReversibleLength cd v board color 1) pieceVectors
    _->return Nothing

--1方向の仮にそこに置いたときにひっくり返せるコマの数
getReversibleLength :: Coordinate -> (Int, Int) -> Board -> Piece -> Int ->IO Int
getReversibleLength cd@(Coordinate (x,y)) (dx,dy) board myColor i=do
  let c=(x+dx*i,y+dy*i)
  if validateCd c then do
    p<-get (newCd c) board
    if p==myColor then return $ i-1
    else if p==reversePiece myColor then getReversibleLength cd (dx,dy) board myColor (i+1)
    else return 0
  else return 0

initBoard :: Board -> IO ()
initBoard board=do
  set (newCd (3,3)) board White
  set (newCd (3,4)) board Black
  set (newCd (4,3)) board Black
  set (newCd (4,4)) board White

printBoard :: Board -> IO ()
printBoard (Board board)=putStrLn "  0 1 2 3 4 5 6 7">>V.freeze board>>=printBoard_ 0 0

printBoard_ :: Int -> Int -> V.Vector Piece -> IO ()
printBoard_ x y fBoard
  |x==8&&y==7=putStrLn "\n"
  |x==8=putChar '\n' >>printBoard_ 0 (y+1) fBoard
  |otherwise=when (x==0) (putStr (show y++" "))>>putStr (show (fBoard V.! (x+y*8)) ++ " ")>>printBoard_ (x+1) y fBoard

--その色がどこかしら置くことが出来かどうか
isPutable :: Piece -> Board -> IO Bool
isPutable color b = anyM [do
      m<-getReversibleLengths (newCd (x,y)) b color
      if isJust m then isAllZeroIOList (fromJust m)>>=(return . not)
      else return False
      |x<-[0..7],y<-[0..7]
   ]

anyM::[IO Bool]->IO Bool
anyM = foldr (\a b->do
  a'<-a
  b'<-b
  return (a'||b')) (return False)