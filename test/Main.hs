{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import CEKMachine
import Data.Bifunctor
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Parser
import Syntax
import Test.Hspec
import Test.QuickCheck hiding (Failure, Fun, Result, Success)
import Text.Trifecta
import TypeChecker

-- ヘルパー関数
mkVar :: String -> Exp
mkVar = Var . BS.pack

mkFun :: [(String, Maybe Ty)] -> Exp -> Exp
mkFun params = Syntax.Fun (map (first BS.pack) params)

mkLet :: String -> Maybe Ty -> Exp -> Exp -> Exp
mkLet x = Let (BS.pack x)

-- テストスイート
cekSpec :: Spec
cekSpec = do
  describe "CEK Machine - Basic Evaluation" $ do
    it "evaluates integers" $ do
      eval (Int 42) `shouldBe` Just (VInt 42)

    it "evaluates booleans" $ do
      eval (Bool True) `shouldBe` Just (VBool True)
      eval (Bool False) `shouldBe` Just (VBool False)

  describe "CEK Machine - Variable Binding" $ do
    it "evaluates let bindings" $ do
      let expr = mkLet "x" Nothing (Int 42) (Var "x")
      eval expr `shouldBe` Just (VInt 42)

    it "handles nested let bindings" $ do
      let expr =
            mkLet "x" Nothing (Int 1) $
              mkLet "y" Nothing (Int 2) $
                mkVar "x"
      eval expr `shouldBe` Just (VInt 1)

    it "handles variable shadowing" $ do
      let expr =
            mkLet "x" Nothing (Int 1) $
              mkLet "x" Nothing (Int 2) $
                mkVar "x"
      eval expr `shouldBe` Just (VInt 2)

  describe "CEK Machine - Function Application" $ do
    it "evaluates simple function application" $ do
      let expr = App (mkFun [("x", Nothing)] (mkVar "x")) [Int 42]
      eval expr `shouldBe` Just (VInt 42)

    it "evaluates multi-argument functions" $ do
      let expr =
            App
              (mkFun [("x", Nothing), ("y", Nothing)] (mkVar "x"))
              [Int 1, Int 2]
      eval expr `shouldBe` Just (VInt 1)

    it "handles higher-order functions" $ do
      let expr =
            mkLet
              "f"
              Nothing
              (mkFun [("x", Nothing)] (mkVar "x"))
              (App (mkVar "f") [Int 42])
      eval expr `shouldBe` Just (VInt 42)

  describe "CEK Machine - Conditional Expressions" $ do
    it "evaluates if-true branch" $ do
      let expr = If (Bool True) (Int 1) (Int 2)
      eval expr `shouldBe` Just (VInt 1)

    it "evaluates if-false branch" $ do
      let expr = If (Bool False) (Int 1) (Int 2)
      eval expr `shouldBe` Just (VInt 2)

  describe "CEK Machine - Error Cases" $ do
    it "handles unbound variables" $ do
      eval (mkVar "x") `shouldBe` Nothing

    it "handles type errors in if conditions" $ do
      let expr = If (Int 42) (Int 1) (Int 2)
      eval expr `shouldBe` Nothing

    it "handles wrong number of arguments" $ do
      let expr = App (mkFun [("x", Nothing)] (mkVar "x")) []
      eval expr `shouldBe` Nothing

  describe "CEK Machine - Complex Programs" $ do
    it "evaluates nested function applications" $ do
      let expr =
            mkLet
              "f"
              Nothing
              ( mkFun
                  [("x", Nothing)]
                  (mkFun [("y", Nothing)] (mkVar "x"))
              )
              (App (App (mkVar "f") [Int 1]) [Int 2])
      eval expr `shouldBe` Just (VInt 1)

    it "handles mutual recursion through let bindings" $ do
      let expr =
            mkLet
              "x"
              Nothing
              (Int 42)
              ( mkLet
                  "f"
                  Nothing
                  (mkFun [("y", Nothing)] (mkVar "x"))
                  (App (mkVar "f") [Int 0])
              )
      eval expr `shouldBe` Just (VInt 42)

checkParse :: (Show a, Eq a) => Result a -> a -> Expectation
checkParse result expect = case result of
  Success actual -> actual `shouldBe` expect
  Failure e -> expectationFailure $ "Parse failed: " ++ show e

parserSpec :: Spec
parserSpec = do
  describe "Parser Basic Tests" $ do
    describe "parseAtomicExp" $ do
      it "parses integer literals" $ do
        checkParse (parseByteString parseAtomicExp mempty "42") (Int 42)
        checkParse (parseByteString parseAtomicExp mempty "-123") (Int (-123))

      it "parses boolean literals" $ do
        checkParse (parseByteString parseAtomicExp mempty "true") (Bool True)
        checkParse (parseByteString parseAtomicExp mempty "false") (Bool False)

      it "parses variables" $ do
        checkParse (parseByteString parseAtomicExp mempty "x") (Var "x")
        checkParse (parseByteString parseAtomicExp mempty "abc") (Var "abc")

      it "rejects reserved words as variables" $ do
        case parseByteString parseAtomicExp mempty "if" of
          Success _ -> expectationFailure "should fail on reserved word 'if'"
          Failure _ -> pure ()
        case parseByteString parseAtomicExp mempty "let" of
          Success _ -> expectationFailure "should fail on reserved word 'let'"
          Failure _ -> pure ()

    describe "parseFun" $ do
      it "parses simple function definitions" $ do
        checkParse
          (parseByteString parseFun mempty "fun x -> x")
          (Fun [("x", Nothing)] (Var "x"))

      it "parses functions with multiple parameters" $ do
        checkParse
          (parseByteString parseFun mempty "fun x y -> x")
          (Fun [("x", Nothing), ("y", Nothing)] (Var "x"))

      it "parses functions with type annotations" $ do
        checkParse
          (parseByteString parseFun mempty "fun (x : Int) -> x")
          (Fun [("x", Just TyInt)] (Var "x"))

        checkParse
          (parseByteString parseFun mempty "fun (x : Int) (y : Bool) -> x")
          (Fun [("x", Just TyInt), ("y", Just TyBool)] (Var "x"))

    describe "parseApp" $ do
      it "parses function application" $ do
        checkParse
          (parseByteString parseApp mempty "f x")
          (App (Var "f") [Var "x"])

      it "parses multiple argument application" $ do
        checkParse
          (parseByteString parseApp mempty "f x y")
          (App (Var "f") [Var "x", Var "y"])

      it "parses nested application" $ do
        checkParse
          (parseByteString parseApp mempty "f (g x)")
          (App (Var "f") [App (Var "g") [Var "x"]])

    describe "parseLet" $ do
      it "parses simple let bindings" $ do
        checkParse
          (parseByteString parseLet mempty "let x = 1 in x")
          (Let "x" Nothing (Int 1) (Var "x"))

      it "parses let with type annotation" $ do
        checkParse
          (parseByteString parseLet mempty "let x : Int = 1 in x")
          (Let "x" (Just TyInt) (Int 1) (Var "x"))

      it "parses nested let expressions" $ do
        checkParse
          (parseByteString parseLet mempty "let x = 1 in let y = 2 in x")
          (Let "x" Nothing (Int 1) (Let "y" Nothing (Int 2) (Var "x")))

    describe "parseIf" $ do
      it "parses simple if expressions" $ do
        checkParse
          (parseByteString parseIf mempty "if true then 1 else 0")
          (If (Bool True) (Int 1) (Int 0))

      it "parses nested if expressions" $ do
        checkParse
          (parseByteString parseIf mempty "if true then if false then 1 else 2 else 3")
          (If (Bool True) (If (Bool False) (Int 1) (Int 2)) (Int 3))

    describe "parseTy" $ do
      it "parses basic types" $ do
        checkParse (parseByteString parseTy mempty "Int") TyInt
        checkParse (parseByteString parseTy mempty "Bool") TyBool

      it "parses function types" $ do
        checkParse (parseByteString parseTy mempty "Int -> Bool") (TyArr TyInt TyBool)
        checkParse
          (parseByteString parseTy mempty "Int -> Bool -> Int")
          (TyArr TyInt (TyArr TyBool TyInt))

      it "parses parenthesized types" $ do
        checkParse
          (parseByteString parseTy mempty "(Int -> Bool) -> Int")
          (TyArr (TyArr TyInt TyBool) TyInt)

    describe "Complex Expressions" $ do
      it "parses function application with let" $ do
        checkParse
          (parseByteString parseExp mempty "let f = fun x -> x in f 42")
          ( Let
              "f"
              Nothing
              (Fun [("x", Nothing)] (Var "x"))
              (App (Var "f") [Int 42])
          )

      it "parses nested expressions with multiple features" $ do
        checkParse
          (parseByteString parseExp mempty "let f = fun (x : Int) -> if x then true else false in f 1")
          ( Let
              "f"
              Nothing
              ( Fun
                  [("x", Just TyInt)]
                  (If (Var "x") (Bool True) (Bool False))
              )
              (App (Var "f") [Int 1])
          )

makeVar :: ByteString -> Exp
makeVar = Var

makeInt :: Integer -> Exp
makeInt = Int

makeBool :: Bool -> Exp
makeBool = Bool

makeFun :: ByteString -> Maybe Ty -> Exp -> Exp
makeFun param ty = Fun [(param, ty)]

-- 任意の式を生成するためのGeneratorを定義
newtype SimpleExp = SimpleExp {getExp :: Exp}
  deriving (Show)

instance Arbitrary SimpleExp where
  arbitrary = SimpleExp <$> sized genExp
    where
      genExp n
        | n <= 0 =
            oneof
              [ Int <$> arbitrary,
                Bool <$> arbitrary,
                pure $ Var "x"
              ]
      genExp n =
        oneof
          [ Int <$> arbitrary,
            Bool <$> arbitrary,
            pure $ Var "x",
            If <$> subExp <*> subExp <*> subExp,
            Let "x" Nothing <$> subExp <*> subExp
          ]
        where
          subExp = genExp (n `div` 2)

-- テストケース集
typeCheckerSpec :: Spec
typeCheckerSpec = do
  describe "TypeChecker" $ do
    describe "Basic type checking" $ do
      it "correctly types integer literals" $ do
        typeCheck (Int 42) `shouldBe` Right TyInt

      it "correctly types boolean literals" $ do
        typeCheck (Bool True) `shouldBe` Right TyBool
        typeCheck (Bool False) `shouldBe` Right TyBool

    describe "Function type checking" $ do
      it "correctly types identity function" $ do
        let idFun = makeFun "x" Nothing (makeVar "x")
        case typeCheck idFun of
          Right (TyArr a b) -> a `shouldBe` b
          _ -> expectationFailure "Identity function should have type a -> a"

      it "correctly types constant function" $ do
        let constFun = makeFun "x" Nothing (makeInt 42)
        case typeCheck constFun of
          Right (TyArr _ TyInt) -> return ()
          _ -> expectationFailure "Constant function should have type a -> Int"

    describe "Application type checking" $ do
      it "correctly types function application" $ do
        let app = App (makeFun "x" Nothing (makeVar "x")) [makeInt 42]
        typeCheck app `shouldBe` Right TyInt

      it "detects type mismatch in function application" $ do
        let app = App (makeFun "x" (Just TyInt) (makeVar "x")) [makeBool True]
        case typeCheck app of
          Left _ -> return ()
          Right _ -> expectationFailure "Should fail with type mismatch"

    describe "Let expressions" $ do
      it "correctly types let binding" $ do
        let letExpr = Let "x" Nothing (makeInt 42) (makeVar "x")
        typeCheck letExpr `shouldBe` Right TyInt

      it "correctly types let with type annotation" $ do
        let letExpr = Let "x" (Just TyInt) (makeInt 42) (makeVar "x")
        typeCheck letExpr `shouldBe` Right TyInt

      it "detects type mismatch in let binding" $ do
        let letExpr = Let "x" (Just TyBool) (makeInt 42) (makeVar "x")
        case typeCheck letExpr of
          Left _ -> return ()
          Right _ -> expectationFailure "Should fail with type mismatch"

    describe "If expressions" $ do
      it "correctly types if expression" $ do
        let ifExpr = If (makeBool True) (makeInt 1) (makeInt 2)
        typeCheck ifExpr `shouldBe` Right TyInt

      it "detects type mismatch in condition" $ do
        let ifExpr = If (makeInt 1) (makeInt 1) (makeInt 2)
        case typeCheck ifExpr of
          Left _ -> return ()
          Right _ -> expectationFailure "Should fail with type mismatch in condition"

      it "detects type mismatch in branches" $ do
        let ifExpr = If (makeBool True) (makeInt 1) (makeBool False)
        case typeCheck ifExpr of
          Left _ -> return ()
          Right _ -> expectationFailure "Should fail with type mismatch in branches"

    describe "Nested functions" $ do
      it "types double-nested identity function" $ do
        let nestedId =
              makeFun "x" Nothing $
                makeFun "y" Nothing $
                  makeVar "y"
        case typeCheck nestedId of
          Right ty -> case ty of
            TyArr _ (TyArr b c) | b == c -> return ()
            _ ->
              expectationFailure $
                "Expected type a -> b -> b, but got: " ++ show ty
          Left e -> expectationFailure e

      it "types nested function with bool condition" $ do
        let nestedBoolFun =
              makeFun "x" Nothing $
                makeFun "y" Nothing $
                  If (makeVar "x") (makeVar "y") (makeVar "y")
        case typeCheck nestedBoolFun of
          Right ty -> case ty of
            TyArr TyBool (TyArr a a') | a == a' -> return ()
            _ ->
              expectationFailure $
                "Expected type Bool -> a -> a, but got: " ++ show ty
          Left e -> expectationFailure e

      it "types nested function with explicit type annotations" $ do
        let annotatedFun =
              makeFun "f" (Just (TyArr TyInt TyInt)) $
                makeFun "x" (Just TyInt) $
                  App (makeVar "f") [makeVar "x"]
        typeCheck annotatedFun
          `shouldBe` Right (TyArr (TyArr TyInt TyInt) (TyArr TyInt TyInt))

      it "types nested function with multiple arguments" $ do
        let multiArgFun =
              makeFun "f" Nothing $
                makeFun "x" Nothing $
                  makeFun "y" Nothing $
                    App (makeVar "f") [makeVar "x", makeVar "y"]
        case typeCheck multiArgFun of
          Right ty -> case ty of
            TyArr (TyArr a (TyArr b c)) (TyArr a' (TyArr b' c')) | a == a' && b == b' && c == c' -> return ()
            _ ->
              expectationFailure $
                "Expected type (a -> b -> c) -> a -> b -> c, but got: " ++ show ty
          Left e -> expectationFailure e

      it "types nested function with let binding" $ do
        let letNestedFun =
              makeFun "x" Nothing $
                makeFun "y" Nothing $
                  Let
                    "z"
                    Nothing
                    (If (makeVar "x") (makeVar "y") (makeVar "y"))
                    (makeVar "z")
        case typeCheck letNestedFun of
          Right ty -> case ty of
            TyArr TyBool (TyArr a a') | a == a' -> return ()
            _ ->
              expectationFailure $
                "Expected type Bool -> a -> a, but got: " ++ show ty
          Left e -> expectationFailure e

      it "types nested function with composition" $ do
        let composeFun =
              makeFun "f" Nothing $
                makeFun "g" Nothing $
                  makeFun "x" Nothing $
                    App (makeVar "f") [App (makeVar "g") [makeVar "x"]]
        case typeCheck composeFun of
          Right ty -> case ty of
            TyArr (TyArr b c) (TyArr (TyArr a b') (TyArr a' c')) | a == a' && b == b' && c == c' -> return ()
            _ ->
              expectationFailure $
                "Expected type (b -> c) -> (a -> b) -> a -> c, but got: " ++ show ty
          Left e -> expectationFailure e

    describe "Property-based tests" $ do
      it "alpha equivalence of functions" $
        property $
          \(NonNegative (_ :: Int)) ->
            let exp1 = makeFun "x" Nothing (makeVar "x")
                exp2 = makeFun "y" Nothing (makeVar "y")
             in typeCheck exp1 == typeCheck exp2

      it "type preservation in if-expressions" $
        property $
          \(NonNegative n) ->
            let mkIfExp b = If (makeBool b) (makeInt n) (makeInt (n + 1))
             in case typeCheck (mkIfExp True) of
                  Right TyInt -> True
                  _ -> False

      it "constant functions preserve types" $
        property $
          \(NonNegative n) ->
            let constFun = makeFun "x" Nothing (makeInt n)
             in case typeCheck constFun of
                  Right (TyArr _ TyInt) -> True
                  _ -> False

-- メインのテスト実行関数
main :: IO ()
main = do
  hspec cekSpec
  hspec parserSpec
  hspec typeCheckerSpec
