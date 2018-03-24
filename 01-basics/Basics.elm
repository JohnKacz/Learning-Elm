module Basics exposing (..)

import Html exposing (..)
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
            "JohnKaczmarek"
    in
        div [] (nameTriangle name)
