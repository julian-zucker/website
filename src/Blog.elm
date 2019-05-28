module Blog exposing (Model, getBlog, posts, view, viewBlogLink)

import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


type alias Model =
    { name : String
    , content : String
    , footerLinks : List String
    }


posts : List Model
posts =
    [ Model "Test1" "Test1 Content" [ "Test2" ]
    , Model "Test2" "Test2 Content" [ "Test1" ]
    ]


getBlog : String -> Maybe Model
getBlog blogName =
    let
        matches =
            List.filter (\p -> p.name == blogName) posts
    in
    case matches of
        [] ->
            Nothing

        post :: _ ->
            Just post



-- Produces the title of the page, and the html that should be rendered as the body of that page.


view : (String -> msg) -> Model -> ( String, List (Html msg) )
view blogVisitMsg { name, content, footerLinks } =
    ( name
    , [ div [ class "blog-content" ]
            [ div [ class "blog-text" ]
                [ text content ]
            , div [ class "blog-footer" ]
                [ ul []
                    (List.map (viewBlogLink blogVisitMsg) footerLinks)
                ]
            ]
      ]
    )


viewBlogLink : (String -> msg) -> String -> Html msg
viewBlogLink sendMsg link =
    li [ class "blog-footer-link" ] [ a [ href ("/blog/" ++ link) ] [ text link ] ]
