module Resume exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : List (Html msg)
view =
    [ embed [ class "resume-embed", src "/assets/Resume.pdf", type_ "application/pdf" ] []
    ]
