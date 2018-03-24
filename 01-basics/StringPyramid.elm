module StringPyramid exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import String


nameTriangle name =
    case name of
        "" ->
            [ br [] [] ]

        _ ->
            [ text name ] ++ [ br [] [] ] ++ nameTriangle (String.dropLeft 1 name)


main =
    let
        name =
            "John Kaczmarek"
    in
        div [ myStyle ] (nameTriangle name)


myStyle =
    style
        [ ( "width", "100%" )
        , ( "height", "40px" )
        , ( "padding", "10px 0" )
        , ( "font-size", "2em" )
        , ( "text-align", "center" )
        , ( "letter-spacing", ".5em" )
        ]
