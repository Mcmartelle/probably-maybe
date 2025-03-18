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
    , d6Whole : String
    , d6Remainder : String
    , coin : Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initOneIn = initialInterRep
        initIr = oneInToIr initOneIn
        initD20 = irToD20 initIr
        initD20Whole = d20ToWhole initD20
        initD20Remainder = d20ToRemainder initOneIn initD20
        initD6 = irToD6 initIr
        initD6Whole = d6ToWhole initD6
        initD6Remainder = d6ToRemainder initOneIn initD6
    in
    (   { oneIn = irToOneIn initialInterRep
        , d20Whole = initD20Whole
        , d20Remainder = initD20Remainder
        , d6Whole = initD6Whole
        , d6Remainder = initD6Remainder
        , coin = irToCoin initialInterRep
        }
    , Cmd.none
    )

initialInterRep : Float
initialInterRep = 80.0


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
        toRemainder 20 oneIn d20


d6ToWhole : Float -> String 
d6ToWhole num =
    floor num
    |> String.fromInt
    

d6ToRemainder : Float -> Float -> String 
d6ToRemainder oneIn logVal =
        toRemainder 6 oneIn logVal


toRemainder : Float -> Float -> Float -> String 
toRemainder diceTotal oneIn logVal =
    let
        val = diceTotal
        whole = floor logVal |> toFloat -- 1
        remainderTotal = oneIn - (val^whole) -- 60
        remainderFraction = remainderTotal / val -- 3
        remainder = ceiling ((remainderTotal * val) / oneIn)
    in
        String.fromInt remainder

    

irToD6 : Float -> Float
irToD6 num =
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
                newD6 = irToD6 newIr
                newD6Whole = d6ToWhole newD6
                newD6Remainder = d6ToRemainder newOneIn newD6
            in
            ( { model
            | oneIn = newOneIn
            , d20Whole = newD20Whole
            , d20Remainder = newD20Remainder
            , d6Whole = newD6Whole
            , d6Remainder = newD6Remainder
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
                , p [] [ text <| model.d6Whole ++ " 6's in a row and a roll greater than " ++ model.d6Remainder ]
                , h3 [] [ text "Coinflips" ]
                , p [] [ text <| String.fromFloat model.coin ]
                ]
            ]
        ]


