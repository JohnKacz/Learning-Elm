module TurnTracker exposing (..)

import Html exposing (..)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (onClick)


main =
    beginnerProgram { model = model, view = view, update = update }



{--

---------- MODEL ----------

--}


type alias Model =
    { currentPlayer : Player
    , nextPlayer : Player
    , otherPlayers : List Player
    , playerCount : Int
    , page : Page
    }


type alias Player =
    { id : Int
    , name : String
    , color : String
    }


type Page
    = SetupPage
    | TrackerPage


colors =
    [ "#EF1010"
    , "#008FFF"
    , "#FAFF00"
    , "#AA00FF"
    , "#00FF00"
    , "#FF00FF"
    , "#FF8B00"
    , "#3E00FF"
    , "#00FF75"
    , "#FF008C"
    , "#00FFFF"
    , "#335533"
    ]


model : Model
model =
    { currentPlayer = { id = 1, name = "Player 1", color = color 1 }
    , nextPlayer = { id = 2, name = "Player 2", color = color 2 }
    , otherPlayers = []
    , playerCount = 2
    , page = SetupPage
    }


color : Int -> String
color n =
    Maybe.withDefault "#FFFFFF" <| List.head <| List.drop (n - 1) colors



{--

---------- UPDATE ----------

--}


type Msg
    = Start
    | NextTurn
    | SkipTurn
    | ReverseTurns
    | Finish
    | Increment
    | Decrement



--type alias TurnMsg
--    = Start | NextTurn | Finish
--type alias PlayerCountMsg
--    = Increment | Decrement


update : Msg -> Model -> Model
update msg model =
    case model.page of
        SetupPage ->
            case msg of
                Start ->
                    { model
                        | otherPlayers = List.reverse model.otherPlayers
                        , page = TrackerPage
                    }

                Increment ->
                    addPlayer model

                Decrement ->
                    removePlayer model

                _ ->
                    model

        TrackerPage ->
            case msg of
                NextTurn ->
                    nextTurn model

                SkipTurn ->
                    skipTurn model

                ReverseTurns ->
                    reverseTurns model

                _ ->
                    model


addPlayer : Model -> Model
addPlayer model =
    if model.playerCount < 12 then
        let
            newPlayerCount =
                model.playerCount + 1

            newPlayer =
                Player newPlayerCount ("Player " ++ (toString newPlayerCount)) (color newPlayerCount)
        in
            { model | otherPlayers = (newPlayer :: model.otherPlayers), playerCount = newPlayerCount }
    else
        model


removePlayer : Model -> Model
removePlayer model =
    if model.playerCount > 2 then
        let
            newPlayerCount =
                model.playerCount - 1
        in
            { model
                | otherPlayers = Maybe.withDefault [] (List.tail model.otherPlayers)
                , playerCount = newPlayerCount
            }
    else
        model


nextTurn : Model -> Model
nextTurn model =
    model |> getNextPlayer


skipTurn : Model -> Model
skipTurn model =
    model |> getNextPlayer |> getNextPlayer


reverseTurns : Model -> Model
reverseTurns model =
    model |> reversePlayers |> getNextPlayer


getNextPlayer : Model -> Model
getNextPlayer model =
    case List.head model.otherPlayers of
        Just player ->
            { model
                | currentPlayer = model.nextPlayer
                , nextPlayer = player
                , otherPlayers = (Maybe.withDefault [] <| List.tail model.otherPlayers) ++ [ model.currentPlayer ]
            }

        Nothing ->
            { model | currentPlayer = model.nextPlayer, nextPlayer = model.currentPlayer }


reversePlayers : Model -> Model
reversePlayers model =
    let
        reversedPlyayers =
            List.reverse (model.nextPlayer :: model.otherPlayers)

        newNext =
            Maybe.withDefault model.nextPlayer <| List.head reversedPlyayers

        newOthers =
            Maybe.withDefault [] <| List.tail reversedPlyayers
    in
        { model
            | nextPlayer = newNext
            , otherPlayers = newOthers
        }



{--

---------- VIEW ----------

--}


view : Model -> Html Msg
view model =
    div [ style [ ( "text-align", "center" ) ] ]
        [ body model

        --, div [] [ text (toString model) ]
        ]


body : Model -> Html Msg
body model =
    case model.page of
        SetupPage ->
            setupPage model

        TrackerPage ->
            trackerPage model


setupPage : Model -> Html Msg
setupPage model =
    div []
        [ h2 [] [ text "Turn Tracker" ]
        , h4 [] [ text "Number of Players" ]
        , div [ style [ ( "font-size", "3em" ) ] ]
            [ button [ onClick Increment, style [ ( "margin", "1em" ), ( "font-size", "20px" ) ] ] [ text "+" ]
            , text (toString model.playerCount)
            , button [ onClick Decrement, style [ ( "margin", "1em" ), ( "font-size", "20px" ) ] ] [ text "-" ]
            ]
        , button [ onClick Start ] [ text "Start" ]
        ]


trackerPage : Model -> Html Msg
trackerPage model =
    div [ trackerStyle model.currentPlayer.color ]
        [ h1 [] [ text ("It is " ++ model.currentPlayer.name ++ "'s turn") ]
        , button [ onClick NextTurn ] [ text "Next" ]
        , button [ onClick SkipTurn ] [ text "Skip" ]
        , button [ onClick ReverseTurns ] [ text "Reverse" ]
        ]


trackerStyle : String -> Attribute Msg
trackerStyle c =
    style
        [ ( "background-color", c )
        , ( "height", "100vh" )
        , ( "padding-top", "30vh" )
        ]
