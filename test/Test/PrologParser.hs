module Test.PrologParser where

import Test.Tasty.HUnit (Assertion, (@?=), assertBool)
import Text.ParserCombinators.Parsec
import Data.Either (isLeft)

import PrologParser
import PrologAst

parseString' :: Parser a -> String -> Either ParseError a
parseString' p = parse (p =>> eof) ""

testParserSuccess :: (Eq a, Show a) => Parser a -> String -> a -> Assertion
testParserSuccess p inp exp =
  parseString' p inp @?= Right exp

testParserFailure :: (Eq a, Show a) => Parser a -> String -> Assertion
testParserFailure p inp =
  assertLeft $ parseString' p inp

assertLeft :: (Show a, Show b) => Either a b -> Assertion
assertLeft x =
  assertBool ("expected: Left\n but got: " ++ show x) (isLeft x)

unit_ident :: Assertion
unit_ident = do
  let parser = identifier
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "abc" "abc"
  success "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
          "aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
  fail "123abc"
  fail "Xyz"

unit_var :: Assertion
unit_var = do
  let parser = var
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "Abc" "Abc"
  success "H" "H"
  success "AabBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
          "AabBcCdDeEfFgGhHiIjJkKlLmMnNoOpPrRsStTuUvVwWxXyYzZ_1234567890"
  fail "123abc"
  fail "xyz"

unit_manyIdent :: Assertion
unit_manyIdent = do
  let parser = many identifier
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a b c" ["a", "b", "c"]

a = Atom "a"
b = Atom "b"
c = Atom "c"
d = Atom "d"

a' = a []
b' = b []
c' = c []
d' = d []

unit_atom :: Assertion
unit_atom = do
  let parser = atom
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a" a'
  success "a b c" (a [l b', l c'])
  success "a (b c)" (a [l $ b [l c']])
  success "a ((b c))" (a [l $ b [l c']])
  success "a ((b c)) d" (a [l $ b [l c'], l d'])
  success "a ((b c))  (d)" (a [l $ b [l c'], l d'])
  success "a ((b  c))  (d)" (a [l $ b [l c'], l d'])
  success "a ((b  c) )  ( d )" (a [l $ b [l c'], l d'])
  success "a((b c))(d)" (a [l $ b [l c'], l d'])
  fail "a (a"
  fail "X a"
  fail "(a)"

l = Left
r = Right

unit_relation :: Assertion
unit_relation = do
  let parser = relation
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a." (Relation a' Nothing)
  success "a b." (Relation (a [l b']) Nothing)
  success "a:-a." (Relation a' (Just (RAtom a')))
  success "a :-a." (Relation a' (Just (RAtom a')))
  success "a:-a b." (Relation a' (Just (RAtom (a [l b']))))
  success "a b:- (a b)  ." (Relation (a [l b']) (Just (RAtom (a [l b']))))
  success "a b:- a;b,c." (Relation (a [l b']) (Just (Disj (RAtom a') (Conj (RAtom b') (RAtom c')))))
  success "a b:- a;(b,c)." (Relation (a [l b']) (Just (Disj (RAtom a') (Conj (RAtom b') (RAtom c')))))
  success "a b:- (a;b),c." (Relation (a [l b']) (Just (Conj (Disj (RAtom a') (RAtom b')) (RAtom c'))))
  success "a b:- a;b;c." (Relation (a [l b']) (Just (Disj (RAtom a') (Disj (RAtom b') (RAtom c')))))
  success "a b:- a,b,c." (Relation (a [l b']) (Just (Conj (RAtom a') (Conj (RAtom b') (RAtom c')))))
  success "a (b (c))  :- (a b) ." (Relation (a [l $ b [l c']]) (Just (RAtom (a [l b']))))

unit_typeExpr :: Assertion
unit_typeExpr = do
  let parser = typeExpr
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "a" (TAtom a')
  success "Y -> X" (Arrow (Var "Y") (Var "X"))
  success "(A -> o) -> list a -> list a -> o" (Arrow (Arrow (Var "A") (TAtom (Atom "o" []))) (Arrow (TAtom (Atom "list" [l $ Atom "a" []])) (Arrow (TAtom $ Atom "list" [l $ Atom "a" []]) (TAtom (Atom "o" [])))))

unit_type :: Assertion
unit_type = do
  let parser = typ
  let success = testParserSuccess parser
  let fail  = testParserFailure parser
  success "type a b." (TypeDef "a" (TAtom b'))
  success "type a b -> X." (TypeDef "a" (Arrow (TAtom b') (Var "X")))
  success "type filter (A -> o) -> list a -> list a -> o." (TypeDef "filter" (Arrow (Arrow (Var "A") (TAtom (Atom "o" []))) (Arrow (TAtom (Atom "list" [l $ Atom "a" []])) (Arrow (TAtom $ Atom "list" [l $ Atom "a" []]) (TAtom (Atom "o" []))))))
  success "type filter (A -> o) -> list A -> list A -> o." (TypeDef "filter" (Arrow (Arrow (Var "A") (TAtom (Atom "o" []))) (Arrow (TAtom (Atom "list" [r "A"])) (Arrow (TAtom $ Atom "list" [r "A"]) (TAtom (Atom "o" []))))))

  fail "type type type -> type."
  fail "type x -> y -> z."
  fail "tupe x o."
