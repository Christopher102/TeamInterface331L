{-
 -  HOPL/CHECKED/Parser.hs
 -
 -  Reference implementation of the toy language CHECKED from the
 -  EOPL3 textbook by Mitchell Wand.
 -
 -  This module provides the grammatical specification for CHECKED.
 -
 -  Author: Matthew A Johnson
 -}
module StatPy.Lang.Parser
  ( parseToplevel,
    ParseError,
  )
where

import StatPy.Type
import StatPy.Lang.Lexer
import StatPy.Lang.Syntax (Exp (..), Pgm (..))
import Text.Parsec (ParseError, choice, eof, many1, parse, sepBy, try)
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.String (Parser)

parseToplevel :: String -> Either ParseError Pgm
parseToplevel = parse (contents toplevel) "<stdin>"

toplevel :: Parser Pgm
toplevel = program

parseExp :: String -> Either ParseError Exp
parseExp = parse (contents expression) "<stdin>"

contents :: Parser a -> Parser a
contents p = do
  whiteSpace
  r <- p
  eof
  return r

{- Grammar for the PROC language -}

program :: Parser Pgm
program = Pgm <$> expression

expression :: Parser Exp
expression =
  (choice . map try)
    [ -- Variable declarations
      LetExp
        <$> (identifier)
        <*> (reserved "=" >> expression),
      LetrecExp
        <$> (reserved "letrec" >> typeAnnotation)
        <*> identifier
        <*> (symbol "(" >> identifier)
        <*> (symbol ":" >> typeAnnotation <* symbol ")")
        <*> (reserved "=" >> expression)
        <*> (reserved "in" >> expression),
      -- Control expressions
      IfExp
        <$> (reserved "if" >> expression)
        <*> (reserved "else" >> expression),
      -- Function definition
      DefExp
        <$> (reserved "def" >> symbol "(" >> identifier)
        <*> (symbol ":" >> typeAnnotation)
        <*> (symbol ")" >> expression),
      -- Function call
      CallExp
        <$> (symbol "(" >> expression)
        <*> (expression <* symbol ")"),
      -- Arithmetic operators
      DiffExp
        <$> (reservedOp "-" >> symbol "(" >> expression)
        <*> (symbol "," >> expression <* symbol ")"),
      -- Arithmetic/numeric predicates
      AddExp
        <$> (symbol "(" >> expression)
        <*> (reservedOp "+" >> expression <* symbol ")"),
      --Mult Exp
      MultExp
        <$> (symbol "(" >> expression)
        <*> (reservedOp "*" >> expression <* symbol ")"),
      DivExp
        <$> (symbol "(" >> expression)
        <*> (reservedOp "/" >> expression <* symbol ")"),
      ExpoExp
        <$> (symbol "(" >> expression)
        <*> (reservedOp "**" >> expression <* symbol ")"),
      ModExp
        <$> (symbol "(" >> expression)
        <*> (reservedOp "%" >> expression <* symbol ")"),
      SqrtExp
        <$> (reserved "sqrt" >> parens expression),
      GreaterExp
        <$> (symbol "(" >> expression)
        <*> (reservedOp ">" >> expression <* symbol ")"),
      LessExp
        <$> (symbol "(" >> expression)
        <*> (reservedOp "<" >> expression <* symbol ")"),
      GreatEqExp
        <$> (symbol "(" >> expression)
        <*> (reservedOp ">=" >> expression <* symbol ")"), 
      LessEqExp
        <$> (symbol "(" >> expression)
        <*> (reservedOp "<=" >> expression <* symbol ")"),  
      EqualExp
        <$> (symbol "(" >> expression)
        <*> (reservedOp "==" >> expression <* symbol ")"),
      NotEqualExp
        <$> (symbol "(" >> expression)
        <*> (reservedOp "!=" >> expression <* symbol ")"),
      NotExp
        <$> (reserved "not" >> parens expression),       
      IsZeroExp
        <$> (reserved "zero?" >> parens expression),
      EmptyExp
        <$ reserved "emptylist",
      List
        <$> (reserved "list" >> symbol "[" >> (sepBy expression (symbol ",") >> "]"),
      -- Integer literal
      ConstExp
        <$> integer,
      -- Variable reference
      VarExp
        <$> identifier
    ]

typeAnnotation :: Parser Type
typeAnnotation =
  (choice . map try)
    [ IntType
        <$ reserved "int",
      BoolType
        <$ reserved "bool",
      ListType 
        <$ reserved "list",
        
      DefType
        <$> (symbol "(" >> typeAnnotation)
        <*> (reservedOp "->" >> typeAnnotation <* symbol ")")
    ]
