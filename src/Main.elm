module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Color exposing (hsl, toCssString)
import Html exposing (..)
import Html.Attributes exposing (class, disabled, style, type_, value, placeholder, autofocus)
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
    { input : String
    , inputError : InputError
    , oneIn : Float
    , percent : String
    , dice : Die
    , d20 : Die
    , d12 : Die
    , d10 : Die
    , d8 : Die
    , d6 : Die
    , d4 : Die
    , coin : Die
    , o50 : Occurrence
    , o80 : Occurrence
    , o90 : Occurrence
    , o95 : Occurrence
    , o99 : Occurrence
    }

type alias Die =
    { title : String
    , whole : String
    , remainder : String
    , remainderExact : Bool
    , val : Int
    }

type alias Occurrence =
    { title : String
    , rolls : String
    , rollsRoundUp : String
    , rollsExact : Bool
    , chance : Float
    }

type InputError
    = AllGood
    | EmptyInput
    | LessThanOne
    | ExactlyOne
    | NotANumber


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initOneIn = initialInterRep
        initIr = oneInToIr initOneIn

        initPercent = String.fromFloat ((1 / initIr) * 100)
        
        d20Val = 20
        initD20 = irToDie d20Val initIr
        initD20Remainder = toRemainder d20Val initIr initD20
        initD20RemainderExact = toRemainderExact d20Val initIr initD20

        d12Val = 12
        initD12 = irToDie d12Val initIr
        initD12Remainder = toRemainder d12Val initIr initD12
        initD12RemainderExact = toRemainderExact d12Val initIr initD12
        
        d10Val = 10
        initD10 = irToDie d10Val initIr
        initD10Remainder = toRemainder d10Val initIr initD10
        initD10RemainderExact = toRemainderExact d10Val initIr initD10
        
        d8Val = 8
        initD8 = irToDie d8Val initIr
        initD8Remainder = toRemainder d8Val initIr initD8
        initD8RemainderExact = toRemainderExact d8Val initIr initD8
        
        d6Val = 6
        initD6 = irToDie d6Val initIr
        initD6Remainder = toRemainder d6Val initIr initD6
        initD6RemainderExact = toRemainderExact d6Val initIr initD6

        d4Val = 4
        initD4 = irToDie d4Val initIr
        initD4Remainder = toRemainder d4Val initIr initD4
        initD4RemainderExact = toRemainderExact d4Val initIr initD4

        coinVal = 2
        initCoin = irToDie coinVal initIr
        initCoinRemainder = toRemainder coinVal initIr initCoin
        initCoinRemainderExact = toRemainderExact coinVal initIr initCoin

        o50Chance = 0.5
        initO50Rolls = toRolls o50Chance initIr
        initO50RollsRoundUp = toRollsRoundUp o50Chance initIr
        initO50RollsExact = toRollsExact o50Chance initIr
        
        o80Chance = 0.8
        initO80Rolls = toRolls o80Chance initIr
        initO80RollsRoundUp = toRollsRoundUp o80Chance initIr
        initO80RollsExact = toRollsExact o80Chance initIr
        
        o90Chance = 0.9
        initO90Rolls = toRolls o90Chance initIr
        initO90RollsRoundUp = toRollsRoundUp o90Chance initIr
        initO90RollsExact = toRollsExact o90Chance initIr
        
        o95Chance = 0.95
        initO95Rolls = toRolls o95Chance initIr
        initO95RollsRoundUp = toRollsRoundUp o95Chance initIr
        initO95RollsExact = toRollsExact o95Chance initIr
        
        o99Chance = 0.99
        initO99Rolls = toRolls o99Chance initIr
        initO99RollsRoundUp = toRollsRoundUp o99Chance initIr
        initO99RollsExact = toRollsExact o99Chance initIr
        
    in
    (   { input = initialInput
        , inputError = AllGood
        , oneIn = irToOneIn initialInterRep
        , percent = initPercent
        , d20 =
            { title = "D20"
            , whole = toWhole initD20
            , remainder =  initD20Remainder
            , remainderExact = initD20RemainderExact
            , val = d20Val
            }
        , dice =
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
        , d10 =
            { title = "D10"
            , whole = toWhole initD10
            , remainder = initD10Remainder
            , remainderExact = initD10RemainderExact
            , val = d10Val
            }
        , d8 =
            { title = "D8"
            , whole = toWhole initD8
            , remainder = initD8Remainder
            , remainderExact = initD8RemainderExact
            , val = d8Val
            }
        , d6 =
            { title = "D6"
            , whole = toWhole initD6
            , remainder = initD6Remainder
            , remainderExact = initD6RemainderExact
            , val = d6Val
            }
        , d4 =
            { title = "D4"
            , whole = toWhole initD4
            , remainder = initD4Remainder
            , remainderExact = initD4RemainderExact
            , val = d4Val
            }
        , coin =
            { title = "Coin Flips"
            , whole = toWhole initCoin
            , remainder = initCoinRemainder
            , remainderExact = initCoinRemainderExact
            , val = coinVal
            }
        , o50 =
            { title = "50%"
            , rolls = initO50Rolls
            , rollsRoundUp = initO50RollsRoundUp
            , rollsExact = initO50RollsExact
            , chance = o50Chance
            }
        , o80 =
            { title = "80%"
            , rolls = initO80Rolls
            , rollsRoundUp = initO80RollsRoundUp
            , rollsExact = initO80RollsExact
            , chance = o80Chance
            }
        , o90 =
            { title = "90%"
            , rolls = initO90Rolls
            , rollsRoundUp = initO90RollsRoundUp
            , rollsExact = initO90RollsExact
            , chance = o90Chance
            }
        , o95 =
            { title = "95%"
            , rolls = initO95Rolls
            , rollsRoundUp = initO95RollsRoundUp
            , rollsExact = initO95RollsExact
            , chance = o95Chance
            }
        , o99 =
            { title = "99%"
            , rolls = initO99Rolls
            , rollsRoundUp = initO99RollsRoundUp
            , rollsExact = initO99RollsExact
            , chance = o99Chance
            }
        }
    , Cmd.none
    )

initialInterRep : Float
initialInterRep = 2.0


initialInput : String
initialInput = "2"


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


toRollsFloat : Float -> Float -> Float
toRollsFloat chance oneIn =
    let
        lnChance = logBase e (1 - chance)
        lnOneIn = logBase e (1 - (1 / oneIn))
    in
        lnChance / lnOneIn


toRolls : Float -> Float -> String
toRolls chance oneIn =
    String.fromFloat <| toRollsFloat chance oneIn


toRollsRoundUp : Float -> Float -> String
toRollsRoundUp chance oneIn =
    toRollsFloat chance oneIn
    |> ceiling
    |> String.fromInt


toRollsExact : Float -> Float -> Bool
toRollsExact chance oneIn =
    toRollsFloat chance oneIn
    |> isWholeNumber

        


-- UPDATE


type Msg
    = ChangeOneIn String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeOneIn newNumber ->
            let
                newInput = newNumber
                newOneIn = case String.toFloat newNumber of
                    Just num ->
                        num
                    Nothing ->
                        -1
                newInputError = case String.toFloat newNumber of
                    Just num ->
                        if num < 1 then LessThanOne
                        else if num == 1 then ExactlyOne
                        else AllGood
                    Nothing ->
                        if String.length newNumber == 0 then EmptyInput else NotANumber

                newIr = oneInToIr newOneIn

                newPercent = String.fromFloat ((1 / newIr) * 100)
                
                oldD20 = model.d20
                newD20 = irToDie model.d20.val newIr
                newD20Whole = toWhole newD20
                newD20Remainder = toRemainder (toFloat model.d20.val) newIr newD20
                newD20RemainderExact = toRemainderExact (toFloat model.d20.val) newIr newD20
        
                oldD12 = model.d12
                newD12 = irToDie model.d12.val newIr
                newD12Whole = toWhole newD12
                newD12Remainder = toRemainder (toFloat model.d12.val) newIr newD12
                newD12RemainderExact = toRemainderExact (toFloat model.d12.val) newIr newD12

                oldD10 = model.d10
                newD10 = irToDie model.d10.val newIr
                newD10Whole = toWhole newD10
                newD10Remainder = toRemainder (toFloat model.d10.val) newIr newD10
                newD10RemainderExact = toRemainderExact (toFloat model.d10.val) newIr newD10

                oldD8 = model.d8
                newD8 = irToDie model.d8.val newIr
                newD8Whole = toWhole newD8
                newD8Remainder = toRemainder (toFloat model.d8.val) newIr newD8
                newD8RemainderExact = toRemainderExact (toFloat model.d8.val) newIr newD8

                oldD6 = model.d6
                oldDice = model.dice
                newD6 = irToDie model.d6.val newIr
                newD6Whole = toWhole newD6
                newD6Remainder = toRemainder (toFloat model.d6.val) newIr newD6
                newD6RemainderExact = toRemainderExact (toFloat model.d6.val) newIr newD6

                oldD4 = model.d4
                newD4 = irToDie model.d4.val newIr
                newD4Whole = toWhole newD4
                newD4Remainder = toRemainder (toFloat model.d4.val) newIr newD4
                newD4RemainderExact = toRemainderExact (toFloat model.d4.val) newIr newD4

                oldCoin = model.coin
                newCoin = irToDie model.coin.val newIr
                newCoinWhole = toWhole newCoin
                newCoinRemainder = toRemainder (toFloat model.coin.val) newIr newCoin
                newCoinRemainderExact = toRemainderExact (toFloat model.coin.val) newIr newCoin

                oldO50 = model.o50
                newO50Rolls = toRolls model.o50.chance newIr
                newO50RollsRoundUp = toRollsRoundUp model.o50.chance newIr
                newO50RollsExact = toRollsExact model.o50.chance newIr
                 
                oldO80 = model.o80
                newO80Rolls = toRolls model.o80.chance newIr
                newO80RollsRoundUp = toRollsRoundUp model.o80.chance newIr
                newO80RollsExact = toRollsExact model.o80.chance newIr
                 
                oldO90 = model.o90
                newO90Rolls = toRolls model.o90.chance newIr
                newO90RollsRoundUp = toRollsRoundUp model.o90.chance newIr
                newO90RollsExact = toRollsExact model.o90.chance newIr
                 
                oldO95 = model.o95
                newO95Rolls = toRolls model.o95.chance newIr
                newO95RollsRoundUp = toRollsRoundUp model.o95.chance newIr
                newO95RollsExact = toRollsExact model.o95.chance newIr
                 
                oldO99 = model.o99
                newO99Rolls = toRolls model.o99.chance newIr
                newO99RollsRoundUp = toRollsRoundUp model.o99.chance newIr
                newO99RollsExact = toRollsExact model.o99.chance newIr
                 
            in
            ( { model
            | input = newInput
            , inputError = newInputError
            , oneIn = newOneIn
            , percent = newPercent
            , d20 = { oldD20
                | whole = newD20Whole
                , remainder = newD20Remainder
                , remainderExact = newD20RemainderExact
                } 
            , dice = { oldDice
                | whole = newD6Whole
                , remainder = newD6Remainder
                , remainderExact = newD6RemainderExact
                } 
            , d12 = { oldD12
                | whole = newD12Whole
                , remainder = newD12Remainder
                , remainderExact = newD12RemainderExact
                } 
            , d10 = { oldD10
                | whole = newD10Whole
                , remainder = newD10Remainder
                , remainderExact = newD10RemainderExact
                } 
            , d8 = { oldD8
                | whole = newD8Whole
                , remainder = newD8Remainder
                , remainderExact = newD8RemainderExact
                } 
            , d6 = { oldD6
                | whole = newD6Whole
                , remainder = newD6Remainder
                , remainderExact = newD6RemainderExact
                } 
            , d4 = { oldD4
                | whole = newD4Whole
                , remainder = newD4Remainder
                , remainderExact = newD4RemainderExact
                } 
            , coin = { oldCoin
                | whole = newCoinWhole
                , remainder = newCoinRemainder
                , remainderExact = newCoinRemainderExact
                } 
            , o50 = { oldO50
                | rolls = newO50Rolls
                , rollsRoundUp = newO50RollsRoundUp
                , rollsExact = newO50RollsExact
                }
            , o80 = { oldO80
                | rolls = newO80Rolls
                , rollsRoundUp = newO80RollsRoundUp
                , rollsExact = newO80RollsExact
                }
            , o90 = { oldO90
                | rolls = newO90Rolls
                , rollsRoundUp = newO90RollsRoundUp
                , rollsExact = newO90RollsExact
                }
            , o95 = { oldO95
                | rolls = newO95Rolls
                , rollsRoundUp = newO95RollsRoundUp
                , rollsExact = newO95RollsExact
                }
            , o99 = { oldO99
                | rolls = newO99Rolls
                , rollsRoundUp = newO99RollsRoundUp
                , rollsExact = newO99RollsExact
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
        [ h1 [] [ text <| titleText model.oneIn ]
        , div
            [ class "item"
            , class "input-item"
            , class "row"
            , class <| case model.inputError of
                AllGood -> "good-input"
                EmptyInput -> "empty-input"
                NotANumber -> "bad-input"
                LessThanOne -> "less-than-one-input"
                ExactlyOne -> "input-exactly-one"
         ]
            [ h2 [] [ text "1" ]
            , span [] [ text "in"]
            , input [ autofocus True, placeholder "Enter a number...", value model.input, onInput ChangeOneIn] []
        ] 
        , div [ class "item-container" ]
            [ percentView model.percent model.inputError
            , coinView model.coin model.inputError
            , diceView model.dice model.inputError
            , diceView model.d20 model.inputError
            , diceView model.d12 model.inputError
            , diceView model.d10 model.inputError
            , diceView model.d8 model.inputError
            , diceView model.d6 model.inputError
            , diceView model.d4 model.inputError
            , occurrenceView model.o50 model.inputError
            , occurrenceView model.o80 model.inputError
            , occurrenceView model.o90 model.inputError
            , occurrenceView model.o95 model.inputError
            , occurrenceView model.o99 model.inputError
            ]
        , p [class "warning"]
            [b [][text "Warning"]
            , text " calculations are using 64 bit floating point numbers and are not infinite precision"
            ]
        ]

titleText : Float -> String
titleText num =
    if num < 0 then "Probably Huh?"
    else if num <= 1 then "Most Definitely"
    else if num < 2 then "Probably"
    else if num == 2 then "Probably Maybe"
    else "Probably Not"




diceView : Die -> InputError -> Html Msg
diceView die err =
    case err of
        AllGood ->
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
        _ -> 
            div [ class "item", class "not-applicable"]
            [ h3 [] [ text die.title ]
            , p [][ text inputNotApplicable ]
            ]
    
    
coinView : Die -> InputError -> Html Msg
coinView die err =
    case err of
        AllGood ->
            let 
                valText = case die.whole of
                    "1" -> "" 
                    _ -> "'s in a row"
            
                showRemainder = not (die.remainder == "0" && die.remainderExact)
            
                remainderText = case (showRemainder, die.remainderExact) of
                    (True, True) -> "and then some"
                    (True, False) -> "and then some"
                    (False, _) -> "exactly"

            in
            div [ class "item"]
            [ h3 [] [ text die.title ]
            , p []
                [ b [] [ text die.whole ]
                , span [] [ text <| " " ++ "head" ++ valText ++ " " ++ remainderText ++ " "]
                ]
            ]

        _ -> 
            div [ class "item", class "not-applicable"]
            [ h3 [] [ text die.title ]
            , p [][ text inputNotApplicable ]
            ]
    
percentView : String -> InputError -> Html Msg
percentView str err =
    case err of
        AllGood -> 
            percentViewGood str

        LessThanOne -> 
            percentViewGood str

        ExactlyOne -> 
            percentViewGood str

        _ ->
            percentViewBad



percentViewGood : String -> Html Msg
percentViewGood str =
    div [ class "item", class "percent"]
    [ h3 [] [ text "Percent" ]
    , p []
        [ b [] [ text <| str ++ "%" ]
        ]
    ]


percentViewBad : Html Msg
percentViewBad =
    div [ class "item", class"percent", class "not-applicable" ]
    [ h3 [] [ text "Percent" ]
    , p [][ text inputNotApplicable ]
    ]


occurrenceView : Occurrence -> InputError -> Html Msg
occurrenceView occ err =
    case err of
        AllGood ->
            let 
                valText = case occ.rollsRoundUp of
                    "1" -> "roll" 
                    _ -> "rolls"
            
                showDecimal = not occ.rollsExact
            
                precisionText = case occ.rollsExact of
                    True -> "exactly to reach a"
                    False -> "to reach a"

            in
            div [ class "item"]
            [ h3 [] [ text occ.title ]
            , p []
                [ b [] [ text occ.rollsRoundUp ]
                , span [] [ text <| " " ++ valText ++ " " ++ precisionText ++ " " ++ occ.title ++ " chance of at least one occurrence "]
                , if showDecimal then span [][ text <| "(" ++ occ.rolls ++ ")" ] else span[][]
                ]
            ]
        _ -> 
            div [ class "item", class "not-applicable"]
            [ h3 [] [ text occ.title ]
            , p [][ text inputNotApplicable ]
            ]


inputNotApplicable : String
inputNotApplicable =
    "Input not applicable"
