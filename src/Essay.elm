module Essay exposing (Model, essays, getEssayByName, view, viewEssayLink, viewEssayPreview)

import Html exposing (..)
import Html.Attributes exposing (..)
import Route


type alias Model =
    { name : String
    , content : String
    }


essays : List Model
essays =
    [ Model "Test1" ""
    , Model "Test2" ""
    ]


getEssayByName : String -> Maybe Model
getEssayByName essayName =
    List.head (List.filter (\p -> p.name == essayName) essays)


view : Model -> ( String, List (Html msg) )
view { name, content } =
    ( name
    , [ div [ class "blog-content" ]
            [ div [ class "blog-text" ]
                [ text content ]
            ]
      ]
    )


viewEssayPreview : Model -> Html msg
viewEssayPreview model =
    div []
        [ p [] [ viewEssayLink model.name ]
        , p [ class "essay-preview" ] [ text (String.left 160 model.content ++ "…") ]
        ]


viewEssayLink : String -> Html msg
viewEssayLink link =
    span [ class "essay-link" ] [ a [ href (Route.toUrlString (Route.Essay link)) ] [ text link ] ]
