module Main exposing (Msg)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Essay
import Html exposing (..)
import Html.Attributes exposing (..)
import Route exposing (Route)
import Url exposing (Url)



---- MODEL ----


type Model
    = Redirect Nav.Key
    | NotFound Nav.Key
    | Home Nav.Key
    | Essays Nav.Key
    | EssayPage Nav.Key Essay.Model
    | Reviews Nav.Key
    | Links Nav.Key


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    changeRouteTo (Route.fromUrl url) (Redirect key)



---- UPDATE ----


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | GotEssayPageMsg Nav.Key String


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

        GotEssayPageMsg key name ->
            case Essay.getEssayByName name of
                Just blog ->
                    ( EssayPage key blog, Cmd.none )

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

                Just route ->
                    case route of
                        Route.Home ->
                            Home key

                        Route.Essays ->
                            Essays key

                        Route.Essay name ->
                            let
                                maybeEssay =
                                    Essay.getEssayByName name
                            in
                            case maybeEssay of
                                Just essay ->
                                    EssayPage key essay

                                Nothing ->
                                    NotFound key

                        Route.Reviews ->
                            Reviews key

                        Route.Links ->
                            Links key
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

        Essays key ->
            key

        EssayPage key _ ->
            key

        Reviews key ->
            key

        Links key ->
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
        , hr [] []
        , div [ class "page-wrapper" ]
            page
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
            [ viewHeaderLink (Route.toUrlString Route.Home) "julianzucker.com"
            , li [] [ text "|" ]
            , viewHeaderLink (Route.toUrlString Route.Essays) "essays"
            , li [] [ text "|" ]
            , viewHeaderLink (Route.toUrlString Route.Reviews) "reviews"
            , li [] [ text "|" ]
            , viewHeaderLink (Route.toUrlString Route.Links) "links"
            ]
        ]


renderPage : Model -> ( String, List (Html Msg) )
renderPage page =
    case page of
        Redirect key ->
            ( "Redirecting", [ text "Redirecting…" ] )

        NotFound key ->
            ( "Not found", [ text "Not found" ] )

        EssayPage key model ->
            Essay.view model

        Home key ->
            ( "Home", [ div [] [ text "Home" ] ] )

        Essays key ->
            ( "Essays"
            , [ div []
                    ([ text "Essay" ]
                        ++ List.map Essay.viewEssayLink (List.map .name Essay.posts)
                    )
              ]
            )

        Reviews key ->
            ( "Reviews"
            , [ div []
                    ([ text "Essay" ]
                        ++ List.map Essay.viewEssayLink (List.map .name Essay.posts)
                    )
              ]
            )

        Links key ->
            ( "Links"
            , [ div []
                    ([ text "Links to various places on the internet:" ]
                        ++ List.map Essay.viewEssayLink (List.map .name Essay.posts)
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
