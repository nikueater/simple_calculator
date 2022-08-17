module Calc exposing (main)

import Browser
import Css exposing (..)
import Data.Expr as Expr exposing (Expr, Op(..))
import Html.Styled exposing (Html, button, div, styled, text, toUnstyled)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
        }


type alias Model =
    { expr : Expr
    }


type Msg
    = InputDigit Int
    | InputOperator Op
    | InputClear
    | InputAllClear
    | InputEq


init : Model
init =
    { expr = Expr.initialize
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputDigit i ->
            { model | expr = Expr.pushDigit model.expr i }

        InputOperator op ->
            { model
                | expr =
                    Expr.pushOperator (Expr.calculate model.expr) op
            }

        InputEq ->
            { model | expr = Expr.calculate model.expr }

        InputClear ->
            { model | expr = Expr.clear model.expr }

        InputAllClear ->
            { model | expr = Expr.initialize }


view : Model -> Html Msg
view model =
    let
        cButton =
            styled button [ flex (int 1), padding (rem 0.5) ]
    in
    div
        [ css
            [ width (px 240)
            , padding (rem 1)
            ]
        ]
        [ div
            [ css
                [ height (rem 2)
                , displayFlex
                , justifyContent flexEnd
                , fontSize (rem 1.2)
                ]
            ]
            [ text (Expr.show model.expr)
            ]
        , div [ css [ displayFlex ] ]
            [ cButton [ onClick InputAllClear ] [ text "AC" ]
            , cButton [ onClick InputClear ] [ text "C" ]
            , cButton [ onClick (InputOperator Mod) ] [ text "%" ]
            ]
        , div [ css [ displayFlex ] ]
            [ cButton [ onClick (InputDigit 7) ] [ text "7" ]
            , cButton [ onClick (InputDigit 8) ] [ text "8" ]
            , cButton [ onClick (InputDigit 9) ] [ text "9" ]
            , cButton [ onClick (InputOperator Add) ] [ text "+" ]
            , cButton [ onClick (InputOperator Sub) ] [ text "-" ]
            ]
        , div [ css [ displayFlex ] ]
            [ cButton [ onClick (InputDigit 4) ] [ text "4" ]
            , cButton [ onClick (InputDigit 5) ] [ text "5" ]
            , cButton [ onClick (InputDigit 6) ] [ text "6" ]
            , cButton [ onClick (InputOperator Mul) ] [ text "*" ]
            , cButton [ onClick (InputOperator Div) ] [ text "/" ]
            ]
        , div [ css [ displayFlex ] ]
            [ cButton [ onClick (InputDigit 0) ] [ text "0" ]
            , cButton [ onClick (InputDigit 1) ] [ text "1" ]
            , cButton [ onClick (InputDigit 2) ] [ text "2" ]
            , cButton [ onClick (InputDigit 3) ] [ text "3" ]
            , cButton [ onClick InputEq ] [ text "=" ]
            ]
        ]
