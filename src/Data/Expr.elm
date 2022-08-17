module Data.Expr exposing (Expr, Op(..), calculate, clear, initialize, pushDigit, pushOperator, show)

{-| simple calculatorの計算の部分を扱うモジュール
-}


{-| 計算式のモデル
-}
type Expr
    = Expr (List Token)


{-| 計算式の初期化
-}
initialize : Expr
initialize =
    Expr []


type BinOp
    = BinOp Int Int Op


type Token
    = Digit Int
    | Operator Op


type Op
    = Add
    | Sub
    | Mul
    | Div
    | Mod


{-| 数値の追加。直前の入力も数字の場合は桁上げする
-}
pushDigit : Expr -> Int -> Expr
pushDigit (Expr xs) digit =
    Expr <|
        case xs of
            (Digit y) :: ys ->
                Digit (y * 10 + digit) :: ys

            _ ->
                Digit digit :: xs


{-| 演算子の追加。直前の入力も演算子の場合は上書きする。数式が空の場合は追加しない。
-}
pushOperator : Expr -> Op -> Expr
pushOperator (Expr xs) op =
    Expr <|
        case xs of
            (Operator _) :: ys ->
                Operator op :: ys

            _ :: _ ->
                Operator op :: xs

            [] ->
                xs


{-| 入力中の数値をクリアする
-}
clear : Expr -> Expr
clear (Expr xs) =
    Expr <|
        case xs of
            (Digit _) :: ys ->
                ys

            _ ->
                xs


{-| 入力中の式を表示する
-}
show : Expr -> String
show (Expr xs) =
    let
        toString token =
            case token of
                Digit i ->
                    String.fromInt i

                Operator Add ->
                    "+"

                Operator Sub ->
                    "-"

                Operator Mul ->
                    "*"

                Operator Div ->
                    "/"

                Operator Mod ->
                    "%"
    in
    xs
        |> List.reverse
        |> List.map toString
        |> String.join " "


{-| 与えられた式が二項演算なら計算する
-}
calculate : Expr -> Expr
calculate expr =
    toBinOp expr
        |> Maybe.map calculateBinOp
        |> Maybe.map (Digit >> List.singleton >> Expr)
        |> Maybe.withDefault expr


toBinOp : Expr -> Maybe BinOp
toBinOp (Expr xs) =
    case xs of
        (Digit rhs) :: (Operator op) :: (Digit lhs) :: [] ->
            Just (BinOp lhs rhs op)

        _ ->
            Nothing


calculateBinOp : BinOp -> Int
calculateBinOp (BinOp lhs rhs op) =
    case op of
        Add ->
            lhs + rhs

        Sub ->
            lhs - rhs

        Mul ->
            lhs * rhs

        Div ->
            lhs // rhs

        Mod ->
            remainderBy rhs lhs
