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
    [ Model "Test1" "Test1 ContentTest1 ContentTest1 ContentTesat1 ContentTest1 ContentTest1 ContentTest1 ContentTest1 ContentTest1 ContentTest1 Content"
    , Model "Test2" "Test2 ContentTest2 ContentTest2 Contentaaaest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 ContentTest2 Contentd"
    ]


getEssayByName : String -> Maybe Model
getEssayByName essayName =
    let
        matches =
            List.filter (\p -> p.name == essayName) essays
    in
    case matches of
        [] ->
            Nothing

        post :: _ ->
            Just post


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
