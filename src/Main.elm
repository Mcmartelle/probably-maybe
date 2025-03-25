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
    , d20 : Die
    , d6 : Die
    , d12 : Die
    , coin : Die
    }

type alias Die =
    { title : String
    , whole : String
    , remainder : String
    , remainderExact : Bool
    , val : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initOneIn = initialInterRep
        initIr = oneInToIr initOneIn
        
        d20Val = 20
        initD20 = irToDie d20Val initIr
        initD20Remainder = toRemainder d20Val initIr initD20
        initD20RemainderExact = toRemainderExact d20Val initIr initD20
        
        d6Val = 6
        initD6 = irToDie d6Val initIr
        initD6Remainder = toRemainder d6Val initIr initD6
        initD6RemainderExact = toRemainderExact d6Val initIr initD6

        d12Val = 12
        initD12 = irToDie d12Val initIr
        initD12Remainder = toRemainder d12Val initIr initD12
        initD12RemainderExact = toRemainderExact d12Val initIr initD12

        coinVal = 2
        initCoin = irToDie coinVal initIr
        initCoinRemainder = toRemainder coinVal initIr initCoin
        initCoinRemainderExact = toRemainderExact coinVal initIr initCoin
        
    in
    (   { oneIn = irToOneIn initialInterRep
        , d20 =
            { title = "D20"
            , whole = toWhole initD20
            , remainder =  initD20Remainder
            , remainderExact = initD20RemainderExact
            , val = d20Val
            }
        , d6 =
            { title = "Six-sided Dice Rolls"
            , whole = toWhole initD6
            , remainder = initD6Remainder
            , remainderExact = initD6RemainderExact
            , val = d6Val
            }
        , d12 =
            { title = "D12"
            , whole = toWhole initD12
            , remainder = initD12Remainder
            , remainderExact = initD12RemainderExact
            , val = d12Val
            }
        , coin =
            { title = "Coin Flips"
            , whole = toWhole initCoin
            , remainder = initCoinRemainder
            , remainderExact = initCoinRemainderExact
            , val = coinVal
            }
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
    



toWhole : Float -> String 
toWhole num =
    floor num
    |> String.fromInt
    

toRemainder : Float -> Float -> Float -> String 
toRemainder diceTotal oneIn logVal =
    let
        val = diceTotal
        whole = floor logVal |> toFloat
        remainderTotal = oneIn - (val^whole)
        remainderFraction = remainderTotal / val
        remainderRaw = (remainderTotal * val) / oneIn
        remainderFloored = floor remainderRaw
        isRemainderWhole = isWholeNumber remainderRaw
        remainder = if remainderFloored == 0 && isRemainderWhole then 0 else remainderFloored + 1
    in
        String.fromInt remainder
        
toRemainderExact : Float -> Float -> Float -> Bool
toRemainderExact diceTotal oneIn logVal =
    let
        val = diceTotal
        whole = floor logVal |> toFloat
        remainderTotal = oneIn - (val^whole)
        remainderFraction = remainderTotal / val
        remainder = (remainderTotal * val) / oneIn
    in
        isWholeNumber remainder


isWholeNumber : Float -> Bool
isWholeNumber num =
    ( num - ( toFloat ( floor num ) ) ) == 0

irToDie : Int -> Float -> Float
irToDie dieMax num =
    logBase (toFloat dieMax)  num


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
                
                oldD20 = model.d20
                newD20 = irToDie model.d20.val newIr
                newD20Whole = toWhole newD20
                newD20Remainder = toRemainder (toFloat model.d20.val) newIr newD20
                newD20RemainderExact = toRemainderExact (toFloat model.d20.val) newIr newD20
        
                oldD6 = model.d6
                newD6 = irToDie model.d6.val newIr
                newD6Whole = toWhole newD6
                newD6Remainder = toRemainder (toFloat model.d6.val) newIr newD6
                newD6RemainderExact = toRemainderExact (toFloat model.d6.val) newIr newD6

                oldD12 = model.d12
                newD12 = irToDie model.d12.val newIr
                newD12Whole = toWhole newD12
                newD12Remainder = toRemainder (toFloat model.d12.val) newIr newD12
                newD12RemainderExact = toRemainderExact (toFloat model.d12.val) newIr newD12

                oldCoin = model.coin
                newCoin = irToDie model.coin.val newIr
                newCoinWhole = toWhole newCoin
                newCoinRemainder = toRemainder (toFloat model.coin.val) newIr newCoin
                newCoinRemainderExact = toRemainderExact (toFloat model.coin.val) newIr newCoin
            in
            ( { model
            | oneIn = newOneIn
            , d20 = { oldD20
                | whole = newD20Whole
                , remainder = newD20Remainder
                , remainderExact = newD20RemainderExact
                } 
            , d6 = { oldD6
                | whole = newD6Whole
                , remainder = newD6Remainder
                , remainderExact = newD6RemainderExact
                } 
            , d12 = { oldD12
                | whole = newD12Whole
                , remainder = newD12Remainder
                , remainderExact = newD12RemainderExact
                } 
            , coin = { oldCoin
                | whole = newCoinWhole
                , remainder = newCoinRemainder
                , remainderExact = newCoinRemainderExact
                } 
            }, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "main", class "wide", class "tall", class "col" ]
        [ h1 [] [ text "Probabilities" ]
        , div [ class "item-container" ]
            [ div [ class "item", class "row" ]
                [ h2 [] [ text "1" ]
                , span [] [ text "in"]
                , input [ value <| String.fromFloat model.oneIn, onInput ChangeOneIn] []
            ] 
            , coinView model.coin
            , diceView model.d6
            , diceView model.d20
            , diceView model.d12
            ]
        ]


diceView : Die -> Html Msg
diceView die =

    let 
        valText = case die.whole of
            "1" -> "" 
            _ -> "'s in a row"
            
        showRemainder = not (die.remainder == "0" && die.remainderExact)
            
        remainderText = case (showRemainder, die.remainderExact) of
            (True, True) -> "and a roll equal to or greater than "
            (True, False) -> "and a roll greater than "
            (False, _) -> "exactly"

    in
    div [ class "item"]
    [ h3 [] [ text die.title ]
    , p []
        [ b [] [ text die.whole ]
        , span [] [ text <| " " ++ (String.fromInt die.val) ++ valText ++ " " ++ remainderText ++ " "]
        , if showRemainder then b [][ text die.remainder ] else span[][]
        ]
    ]
    
    
coinView : Die -> Html Msg
coinView die =

    let 
        valText = case die.whole of
            "1" -> "" 
            _ -> "'s in a row"
            
        showRemainder = not (die.remainder == "0" && die.remainderExact)
            
        remainderText = case (showRemainder, die.remainderExact) of
            (True, True) -> "and a bit more"
            (True, False) -> "and a bit more"
            (False, _) -> "exactly"

    in
    div [ class "item"]
    [ h3 [] [ text die.title ]
    , p []
        [ b [] [ text die.whole ]
        , span [] [ text <| " " ++ "head" ++ valText ++ " " ++ remainderText ++ " "]
        ]
    ]
    
