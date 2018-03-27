module TurnTracker exposing (..)

import Html exposing (Html, beginnerProgram, div, text, button)
import Html.Events exposing (onClick)


main =
    beginnerProgram { model = model, view = view, update = update }


type alias Model =
    { currentPlayer : Player
    , nextPlayer : Player
    , otherPlayers : List Player
    , playerCount : Int
    , status : String
    }


type alias Player =
    { id : Int
    , name : String
    }


model : Model
model =
    { currentPlayer = { id = 1, name = "Player 1" }
    , nextPlayer = { id = 2, name = "Player 2" }
    , otherPlayers = []
    , playerCount = 2
    }


type Msg
    = Start
    | NextTurn
    | Finish
    | Increment
    | Decrement



--type alias TurnMsg
--    = Start | NextTurn | Finish
--type alias PlayerCountMsg
--    = Increment | Decrement


update : Msg -> Model -> Model
update msg model =
    case msg of
        -- TODO - Handle Start after game has already been started
        Start ->
            { model
                | otherPlayers = List.reverse model.otherPlayers
            }

        Finish ->
            model

        Increment ->
            addPlayer model

        Decrement ->
            removePlayer model


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


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ text "Number of Players"
            , button [ onClick Increment ] [ text "+" ]
            , text (toString model.playerCount)
            , button [ onClick Decrement ] [ text "-" ]
            ]
        , button [ onClick Start ] [ text "Start" ]
        , div [] [ text (toString model) ]
        ]
