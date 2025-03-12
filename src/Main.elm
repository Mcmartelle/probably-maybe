module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Color exposing (hsl, toCssString)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, style, type_, value)
import Html.Events exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { oneIn : Float
    , d20Whole : String
    , d20Remainder : String
    , dice : Float
    , coin : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    (   { oneIn = irToOneIn initialInterRep
        , d20Whole = ""
        , d20Remainder = ""
        , dice = irToDice initialInterRep
        , coin = irToCoin initialInterRep
        }
    , Cmd.none
    )

initialInterRep : Float
initialInterRep = 0.01


irToOneIn : Float -> Float
irToOneIn num = 
    num
    

oneInToIr : Float -> Float
oneInToIr num = 
    num
    

irToD20 : Float -> Float
irToD20 num =
    logBase 20 num


d20ToWhole : Float -> String 
d20ToWhole num =
    floor num
    |> String.fromInt
    

d20ToRemainder : Float -> Float -> String 
d20ToRemainder oneIn d20 =
    let
        whole = floor d20
        remainder = (floor oneIn) - (20^whole)
    in
        String.fromInt remainder

    

irToDice : Float -> Float
irToDice num =
    logBase 6 num    


irToCoin : Float -> Float
irToCoin num =
    logBase 2 num    


-- UPDATE


type Msg
    = ChangeOneIn String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeOneIn newNumber ->
            let
                newOneIn = case String.toFloat newNumber of
                    Just num ->
                        num
                    Nothing ->
                        0.0
                newIr = oneInToIr newOneIn
                newD20 = irToD20 newIr
                newD20Whole = d20ToWhole newD20
                newD20Remainder = d20ToRemainder newOneIn newD20
            in
            ( { model
            | oneIn = newOneIn
            , d20Whole = newD20Whole
            , d20Remainder = newD20Remainder
            }, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "wide", class "tall", class "col" ]
        [ h1 [] [ text "Probabilities" ]
        , div [ class "col", style "flex-wrap" "wrap" ]
            [ div [ class "col" ]
                [ h3 [] [ text "1 in X" ]
                , input [ value <| String.fromFloat model.oneIn, onInput ChangeOneIn] []
                , p [] [ text <| String.fromFloat model.oneIn ]
                , h3 [] [ text "D20" ]
                , p [] [ text <| model.d20Whole ++ " 20's in a row and a roll greater than " ++ model.d20Remainder ]
                , h3 [] [ text "Dice" ]
                , p [] [ text <| String.fromFloat model.dice ]
                , h3 [] [ text "Coinflips" ]
                , p [] [ text <| String.fromFloat model.coin ]
                ]
            ]
        ]


