module Resume exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : List (Html msg)
view =
    [ span [] [ text "Unless I handed you a resume in person recently, the resume below is hideously out of date." ]
    , embed [ class "resume-embed", src "/assets/Resume.pdf", type_ "application/pdf" ] []
    ]
