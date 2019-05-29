module Main exposing (Msg)

import Blog
import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Url exposing (Url)



---- MODEL ----


type Model
    = Redirect Nav.Key
    | NotFound Nav.Key
    | Home Nav.Key
    | BlogHome Nav.Key
    | Blog Nav.Key Blog.Model


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Route.fromUrl url) (Redirect key)



---- UPDATE ----


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotBlogPageMsg Nav.Key String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink (Browser.Internal url) ->
            ( model
            , Nav.pushUrl (navKey model) (Url.toString url)
            )

        ClickedLink (Browser.External url) ->
            ( model, Nav.load url )

        ChangedUrl url ->
            changeRouteTo (Route.fromUrl url) model

        GotBlogPageMsg key name ->
            case Blog.getBlog name of
                Just blog ->
                    ( Blog key blog, Cmd.none )

                Nothing ->
                    ( NotFound key, Cmd.none )


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        key =
            navKey model

        newModel =
            case maybeRoute of
                Nothing ->
                    NotFound key

                Just Route.Home ->
                    Home key

                Just Route.BlogHome ->
                    BlogHome key

                Just (Route.Blog name) ->
                    let
                        maybeBlog =
                            Blog.getBlog name
                    in
                    case maybeBlog of
                        Just blog ->
                            Blog key blog

                        Nothing ->
                            NotFound key
    in
    ( newModel, Cmd.none )


navKey : Model -> Nav.Key
navKey model =
    case model of
        Redirect key ->
            key

        NotFound key ->
            key

        Home key ->
            key

        BlogHome key ->
            key

        Blog key _ ->
            key



---- VIEW ----


view : Model -> Document Msg
view model =
    let
        ( title, page ) =
            renderPage model
    in
    { title = title
    , body =
        [ header model
        , div [ class "page" ]
            ([ hr [] [] ] ++ page)
        ]
    }


header : Model -> Html Msg
header model =
    let
        viewHeaderLink : String -> String -> Html Msg
        viewHeaderLink pageLink pageName =
            li [] [ a [ href pageLink ] [ text pageName ] ]
    in
    div [ class "header" ]
        [ ul []
            [ viewHeaderLink "/" "julianzucker.com"
            , viewHeaderLink "/blog/" "/blog/"
            ]
        ]


renderPage : Model -> ( String, List (Html Msg) )
renderPage page =
    case page of
        Redirect key ->
            ( "Redirecting", [ text "Redirecting…" ] )

        NotFound key ->
            ( "Not found", [ text "Not found" ] )

        Blog key model ->
            Blog.view (GotBlogPageMsg key) model

        Home key ->
            ( "Home", [ div [] [ text "Home" ] ] )

        BlogHome key ->
            ( "Blog"
            , [ div []
                    ([ text "Blog" ]
                        ++ List.map (Blog.viewBlogLink (GotBlogPageMsg key)) (List.map .name Blog.posts)
                    )
              ]
            )



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        , subscriptions = always Sub.none
        , update = update
        , view = view
        }
