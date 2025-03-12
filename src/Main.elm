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
    , d20 : Float
    , dice : Float
    , coin : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    (   { oneIn = irToOneIn initialInterRep
        , d20 = irToD20 initialInterRep
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
                newDice = irToDice newIr
                newCoin = irToCoin newIr
            in
            ( { model
            | oneIn = newOneIn
            , d20 = newD20
            , dice = newDice
            , coin = newCoin
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
                , p [] [ text <| String.fromFloat model.d20 ]
                , h3 [] [ text "Dice" ]
                , p [] [ text <| String.fromFloat model.dice ]
                , h3 [] [ text "Coinflips" ]
                , p [] [ text <| String.fromFloat model.coin ]
                ]
            ]
        ]


