{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module MatrixMarket (Matrix(..), readMatrix) where

import Control.Applicative                      hiding ( many )

import Data.Int
import Data.Complex
import Data.Attoparsec.ByteString.Char8
import Data.ByteString.Lex.Fractional
import qualified Data.Attoparsec.Lazy           as L
import qualified Data.ByteString.Lazy           as L


-- | Specifies the element type.  Pattern matrices do not have any elements,
-- only indices, and only make sense for coordinate matrices and vectors.
--
data Field  = Real | Complex | Integer | Pattern
    deriving (Eq, Show)

-- | Specifies either sparse or dense storage.  In sparse (\"coordinate\")
-- storage, elements are given in (i,j,x) triplets for matrices (or (i,x) for
-- vectors).  Indices are 1-based, so that A(1,1) is the first element of a
-- matrix, and x(1) is the first element of a vector.
--
-- In dense (\"array\") storage, elements are given in column-major order.
--
-- In both cases, each element is given on a separate line.
--
data Format = Coordinate | Array
    deriving (Eq, Show)

-- | Specifies any special structure in the matrix.  For symmetric and hermition
-- matrices, only the lower-triangular part of the matrix is given. For skew
-- matrices, only the entries below the diagonal are stored.
--
data Structure = General | Symmetric | Hermitian | Skew
    deriving (Eq, Show)


-- We really want a type parameter to Matrix, but I think that requires some
-- kind of dynamic typing so that we can determine (a ~ Integral) or (a ~
-- RealFloat), and so forth, depending on the file being read. This will do for
-- our purposes...
--
-- Format is: (rows,columns) nnz [(row,column,value)]
--
data Matrix
  = PatternMatrix (Int,Int) Int [(Int32,Int32)]
  | IntMatrix     (Int,Int) Int [(Int32,Int32,Int)]
  | RealMatrix    (Int,Int) Int [(Int32,Int32,Float)]
  | ComplexMatrix (Int,Int) Int [(Int32,Int32,Complex Float)]
  deriving Show


--------------------------------------------------------------------------------
-- Combinators
--------------------------------------------------------------------------------

comment :: Parser ()
comment = char '%' *> skipWhile (not . eol) *> endOfLine
  where
    eol w = w `elem` ("\n\r" :: String)

floating :: Fractional a => Parser a
floating = do
  mv <- readDecimal <$> (skipSpace *> takeTill isSpace)  -- readDecimal does the fancy stuff
  case mv of
       Just (v,_) -> return v
       Nothing    -> fail "floating-point number"

integral :: Integral a => Parser a
integral = skipSpace *> decimal

format :: Parser Format
format =  string "coordinate" *> pure Coordinate
      <|> string "array"      *> pure Array
      <?> "matrix format"

field :: Parser Field
field =  string "real"    *> pure Real
     <|> string "complex" *> pure Complex
     <|> string "integer" *> pure Integer
     <|> string "pattern" *> pure Pattern
     <?> "matrix field"

structure :: Parser Structure
structure =  string "general"        *> pure General
         <|> string "symmetric"      *> pure Symmetric
         <|> string "hermitian"      *> pure Hermitian
         <|> string "skew-symmetric" *> pure Skew
         <?> "matrix structure"

header :: Parser (Format,Field,Structure)
header =  string "%%MatrixMarket matrix"
       >> (,,) <$> (skipSpace *> format)
               <*> (skipSpace *> field)
               <*> (skipSpace *> structure)
               <*  endOfLine
               <?> "MatrixMarket header"

extent :: Parser (Int,Int,Int)
extent = do
  [m,n,l] <- skipWhile isSpace *> count 3 integral <* endOfLine
  return (m,n,l)

line :: Integral i => Parser a -> Parser (i,i,a)
line f = (,,) <$> integral
              <*> integral
              <*> f
              <*  endOfLine

--------------------------------------------------------------------------------
-- Matrix Market
--------------------------------------------------------------------------------

matrix :: Parser Matrix
matrix = do
  (_,t,_) <- header
  (m,n,l) <- skipMany comment *> extent
  case t of
    Real    -> RealMatrix    (m,n) l `fmap` many1 (line floating)
    Complex -> ComplexMatrix (m,n) l `fmap` many1 (line ((:+) <$> floating <*> floating))
    Integer -> IntMatrix     (m,n) l `fmap` many1 (line integral)
    Pattern -> PatternMatrix (m,n) l `fmap` many1 ((,) <$> integral <*> integral)


readMatrix :: FilePath -> IO Matrix
readMatrix file = do
  chunks <- L.readFile file
  case L.parse matrix chunks of
    L.Fail _ _ msg      -> error $ file ++ ": " ++ msg
    L.Done _ mtx        -> return mtx

