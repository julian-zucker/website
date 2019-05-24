module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)



---- MODEL ----


type alias Model =
    { page : Page
    }


init : ( Model, Cmd Msg )
init =
    ( { page = Home }, Cmd.none )



---- UPDATE ----


type Page
    = Home
    | Blog


type Msg
    = MenuItemClick Page


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MenuItemClick page ->
            ( { model | page = page }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ sidebar model.page
        , div [ class "page" ]
            (render_page model.page)
        ]


sidebar : Page -> Html Msg
sidebar page =
    let
        attributes page_link =
            if page_link == page then
                [ onClick (MenuItemClick page_link)
                , class "selected"
                ]

            else
                [ onClick (MenuItemClick page_link) ]
    in
    ul [ class "sidenav" ]
        [ li []
            [ img [ src "assets/img/profile.jpg" ] []
            ]
        , li (attributes Home)
            [ text "Home" ]
        , li (attributes Blog) [ text "Blog" ]
        ]


render_page : Page -> List (Html Msg)
render_page page =
    case page of
        Home ->
            [ div [] [ text "Home" ] ]

        Blog ->
            [ div [] [ text "Blog" ] ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
