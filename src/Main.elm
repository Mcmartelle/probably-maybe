module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Color exposing (hsl, toCssString)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, style, type_, value)
import Html.Events exposing (..)
import Svg exposing (Svg, circle, line, rect, svg)
import Svg.Attributes exposing (cx, cy, height, r, rx, ry, viewBox, width, x, x1, x2, y, y1, y2)



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
    { seed : Float
    , goldenRatioValues : List Float
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { seed = 0.0
      , goldenRatioValues = [ 0.0 ]
      }
    , Cmd.none
    )


goldenRatio : Float
goldenRatio =
    (1 + sqrt 5) / 2




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
        [ h1 [] [ text "Ratios" ]
        , button [ onClick IncrementColors ] [ text "Add a Color" ]
        , div [ class "col", style "flex-wrap" "wrap" ]
            [ div [ class "col" ]
                [ h3 [] [ text "Golden Ratio" ]
                , svg
                    [ width "360"
                    , height "360"
                    , viewBox "0 0 360 360"
                    ]
                    [ circle
                        [ cx "180"
                        , cy "180"
                        , r "180"
                        ]
                        []
                    , svg [] <| List.map colorDialIndicator model.goldenRatioValues
                    ]
                ]
            ]
        ]


colorNumberItem : Float -> Html Msg
colorNumberItem n =
    div [ style "background-color" (hsl (n * 360.0) 0.99 0.5 |> Color.toCssString) ]
        [ String.fromFloat n |> text ]


colorDialIndicator : Float -> Svg Msg
colorDialIndicator n =
    line
        [ x1 "180"
        , y1 "180"
        , x2 "180"
        , y2 "14"
        , Svg.Attributes.stroke (hsl (n * 360.0) 0.99 0.5 |> Color.toCssString)
        , Svg.Attributes.strokeWidth "8"
        , Svg.Attributes.strokeLinecap "round"
        , Svg.Attributes.transform ("rotate(" ++ (n * 360.0 |> String.fromFloat) ++ " 180 180)")
        ]
        []
