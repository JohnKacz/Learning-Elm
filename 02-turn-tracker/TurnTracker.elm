module TurnTracker exposing (..)

import Html exposing (..)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)


main =
    beginnerProgram { model = model, view = view, update = update }



--colors =
--    [ "#F70044"
--    , "#A52A2A"
--    , "#F6D600"
--    , "#167FC5"
--    , "#11ED76"
--    , "#8A2BE2"
--    , "#F76F22"
--    , "#BB11AA"
--    , "#33EEEE"
--    , "#333333"
--    ]


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
    }


type Page
    = SetupPage
    | TrackerPage


model : Model
model =
    { currentPlayer = { id = 1, name = "Player 1" }
    , nextPlayer = { id = 2, name = "Player 2" }
    , otherPlayers = []
    , playerCount = 2
    , page = SetupPage
    }


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
                Player newPlayerCount ("Player " ++ (toString newPlayerCount))
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
    case List.head model.otherPlayers of
        Just player ->
            { model
                | currentPlayer = model.nextPlayer
                , nextPlayer = player
                , otherPlayers = (Maybe.withDefault [] <| List.tail model.otherPlayers) ++ [ model.currentPlayer ]
            }

        Nothing ->
            { model | currentPlayer = model.nextPlayer, nextPlayer = model.currentPlayer }


skipTurn : Model -> Model
skipTurn model =
    model |> nextTurn |> nextTurn


reverseTurns : Model -> Model
reverseTurns model =
    model |> reversedPlayers |> nextTurn


reversedPlayers : Model -> Model
reversedPlayers model =
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


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Turn Tracker" ]
        , hr [] []
        , case model.page of
            SetupPage ->
                setupPage model

            TrackerPage ->
                trackerPage model
        , div [] [ text (toString model) ]
        ]


setupPage : Model -> Html Msg
setupPage model =
    div []
        [ div []
            [ text "Number of Players"
            , button [ onClick Increment ] [ text "+" ]
            , text (toString model.playerCount)
            , button [ onClick Decrement ] [ text "-" ]
            ]
        , button [ onClick Start ] [ text "Start" ]
        ]


trackerPage : Model -> Html Msg
trackerPage model =
    div []
        [ h4 [] [ text ("It is " ++ model.currentPlayer.name ++ "'s turn") ]
        , button [ onClick NextTurn ] [ text "Next" ]
        , button [ onClick SkipTurn ] [ text "Skip" ]
        , button [ onClick ReverseTurns ] [ text "Reverse" ]
        ]
