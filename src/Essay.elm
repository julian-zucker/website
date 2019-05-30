module Essay exposing (Model, getEssayByName, posts, view, viewEssayLink)

import Html exposing (..)
import Html.Attributes exposing (..)
import Route


type alias Model =
    { name : String
    , content : String
    }


posts : List Model
posts =
    [ Model "Test1" "Test1 Content"
    , Model "Test2" "Test2 Content"
    ]


getEssayByName : String -> Maybe Model
getEssayByName blogName =
    let
        matches =
            List.filter (\p -> p.name == blogName) posts
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


viewEssayLink : String -> Html msg
viewEssayLink link =
    li [ class "essay-link" ] [ a [ href (Route.toUrlString (Route.Essay link)) ] [ text link ] ]
