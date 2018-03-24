module DynamicStringPyramid exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, style)
import Html.Events exposing (onInput)
import String


main =
    beginnerProgram { model = "", view = view, update = update }



-- UPDATE


type Msg
    = NewContent String


update (NewContent content) oldContent =
    content



-- VIEW


view content =
    div []
        [ input [ placeholder "Tell me something funny", onInput NewContent, myStyle ] []
        , div [ myStyle ] (sTriangle content)
        ]


myStyle =
    style
        [ ( "width", "100%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        , ( "letter-spacing", ".5em" )
        ]


sTriangle s =
    case s of
        "" ->
            [ br [] [] ]

        _ ->
            [ text s ] ++ [ br [] [] ] ++ sTriangle (String.dropLeft 1 s)
