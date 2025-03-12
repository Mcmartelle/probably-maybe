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
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { oneIn = 0.01
      , 
      }
    , Cmd.none
    )


oneInToD20 : 


-- UPDATE


type Msg
    = IncrementColors
    | DecrementColors


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IncrementColors ->
            let
                nextGoldenRatioValue =
                    case List.head model.goldenRatioValues of
                        Just value ->
                            calculateNext value goldenRatio

                        Nothing ->
                            model.seed

            in
            ( { model | goldenRatioValues = [ nextGoldenRatioValue ] ++ model.goldenRatioValues }, Cmd.none )

        DecrementColors ->
            ( { model
                | goldenRatioValues =
                    case List.tail model.goldenRatioValues of
                        Just tailValues ->
                            tailValues

                        Nothing ->
                            []
              }
            , Cmd.none
            )


calculateNext : Float -> Float -> Float
calculateNext prev ratio =
    let
        sum =
            prev + ratio
    in
    sum - (floor sum |> toFloat)



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
                ]
            ]
        ]


