module Calc exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias Model =
    { result : Int
    , input : Int
    , display : String
    , operator : Op
    }


type Op
    = Plus
    | Minus
    | Times
    | Devide
    | Modulo
    | Equal
    | None


type Msg
    = PushNum Int
    | PushOp Op
    | PushEqual
    | PushClear
    | PushAllClear


init : Model
init =
    { result = 0
    , input = -1
    , display = ""
    , operator = None
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PushNum num ->
            updateInputNumber num model

        PushOp op ->
            updatePushOperator op model

        PushEqual ->
            updatePushEqual model

        PushClear ->
            { model
                | input = -1
                , display = ""
            }

        PushAllClear ->
            init


updateInputNumber : Int -> Model -> Model
updateInputNumber number model =
    if model.result /= 0 && model.operator == None then
        model

    else if model.input == -1 then
        inputNumberSub number model

    else
        inputNumberSub (model.input * 10 + number) model


inputNumberSub : Int -> Model -> Model
inputNumberSub number model =
    { model
        | input = number
        , display = String.fromInt number
    }


{-| 演算子が押された際に考慮すべきケース

1.  modelがinitと同じ状態の場合
2.  数字入力後に初めて演算子が押された場合
3.  2度目以降の数字入力後に演算子が押された場合
4.  イコールの後に追加で演算子が押された場合と、演算子を押した後に別の演算子に変更する場合
5.  上記の条件を抜けてきてmodelのinputが-1の場合
6.  概ね計算実行ができる場合

-}
updatePushOperator : Op -> Model -> Model
updatePushOperator op model =
    if model == init then
        model

    else if model.result == 0 && model.input /= -1 then
        { model
            | result = model.input
            , input = -1
            , display = String.fromInt model.input
            , operator = op
        }

    else if model.result /= 0 && model.input /= -1 then
        updateCalcExcute op model

    else if model.operator == Equal || model.operator /= op then
        { model | operator = op }

    else if model.input == -1 then
        model

    else
        updateCalcExcute op model


updatePushEqual : Model -> Model
updatePushEqual model =
    if model.operator == None then
        model

    else if model.input == -1 then
        case model.operator of
            Times ->
                if model.display == "0" then
                    updateCalcExcute Equal model

                else
                    model

            Devide ->
                if model.display == "0" then
                    updateCalcExcute Equal model

                else
                    model

            _ ->
                model

    else
        updateCalcExcute Equal model


updateCalcExcute : Op -> Model -> Model
updateCalcExcute op model =
    case model.operator of
        Plus ->
            calcSub (model.result + model.input) op model

        Minus ->
            if model.result < model.input then
                calcSub 0 op model

            else
                calcSub (model.result - model.input) op model

        Times ->
            calcSub (model.result * model.input) op model

        Devide ->
            calcSub (model.result // model.input) op model

        Modulo ->
            calcSub (remainderBy model.input model.result) op model

        _ ->
            model


calcSub : Int -> Op -> Model -> Model
calcSub result op model =
    { model
        | result = result
        , input = -1
        , display = String.fromInt result
        , operator = op
    }


stringFromOp : Op -> String
stringFromOp op =
    case op of
        Plus ->
            "+"

        Minus ->
            "-"

        Times ->
            "×"

        Devide ->
            "÷"

        Modulo ->
            "%"

        _ ->
            ""


view : Model -> Html Msg
view model =
    div []
        [ div [ class "display" ] [ text model.display ]
        , div [ class "display" ] [ text (stringFromOp model.operator) ]
        , div []
            [ button [ onClick PushAllClear ] [ text "AC" ]
            , button [ onClick PushClear ] [ text "C" ]
            , button [ onClick (PushOp Modulo) ] [ text "%" ]
            ]
        , div []
            [ button [ onClick (PushNum 7) ] [ text "7" ]
            , button [ onClick (PushNum 8) ] [ text "8" ]
            , button [ onClick (PushNum 9) ] [ text "9" ]
            , button [ onClick (PushOp Plus) ] [ text "+" ]
            , button [ onClick (PushOp Minus) ] [ text "-" ]
            ]
        , div []
            [ button [ onClick (PushNum 4) ] [ text "4" ]
            , button [ onClick (PushNum 5) ] [ text "5" ]
            , button [ onClick (PushNum 6) ] [ text "6" ]
            , button [ onClick (PushOp Times) ] [ text "×" ]
            , button [ onClick (PushOp Devide) ] [ text "÷" ]
            ]
        , div []
            [ button [ onClick (PushNum 0) ] [ text "0" ]
            , button [ onClick (PushNum 1) ] [ text "1" ]
            , button [ onClick (PushNum 2) ] [ text "2" ]
            , button [ onClick (PushNum 3) ] [ text "3" ]
            , button [ onClick PushEqual ] [ text "=" ]
            ]
        ]


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }
