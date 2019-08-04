module Main exposing (Msg)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Essay
import Html exposing (..)
import Html.Attributes exposing (..)
import Log
import Resume
import Route exposing (Route)
import Url exposing (Url)



---- MODEL ----


type Model
    = Redirect Nav.Key
    | NotFound Nav.Key
    | Home Nav.Key
    | Essays Nav.Key
    | EssayPage Nav.Key Essay.Model
    | Log Nav.Key
    | LogEntry Nav.Key Log.Model
    | NoSuchLogEntry Nav.Key
    | Resume Nav.Key


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
            case Essay.getEssayBySlug name of
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
                                    Essay.getEssayBySlug name
                            in
                            case maybeEssay of
                                Just essay ->
                                    EssayPage key essay

                                Nothing ->
                                    NotFound key

                        Route.Log ->
                            Log key

                        Route.LogEntry weekNumber ->
                            let
                                maybeLog =
                                    Log.getLogForWeek weekNumber
                            in
                            case maybeLog of
                                Just log ->
                                    LogEntry key log

                                Nothing ->
                                    NoSuchLogEntry key

                        Route.Resume ->
                            Resume key
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

        Log key ->
            key

        LogEntry key _ ->
            key

        NoSuchLogEntry key ->
            key

        Resume key ->
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
            (List.intersperse (li [] [ text "|" ])
                [ viewHeaderLink (Route.toUrlString Route.Home) "julianzucker.com"
                , viewHeaderLink (Route.toUrlString Route.Resume) "resume"
                , viewHeaderLink (Route.toUrlString Route.Essays) "essays"
                , viewHeaderLink (Route.toUrlString Route.Log) "log"
                ]
            )
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
            ( "Julian Zucker's Website"
            , [ div []
                    [ p []
                        [ p [] [ text """Hi! Welcome to my website!""" ]
                        , p []
                            [ text """I like teaching computer science, thinking about evolution,
                                         and writing code. My short-term goals are to grow as a software developer,
                                         create software that people can use, and publish research while I'm still
                                         an undergraduate. My longer-term goals are to get a job teaching computer
                                         science to competent students (although I'm not sure if professorship meets
                                         this definition), improve the productivity of software engineers
                                         by 1% through improving tooling/education/methodology, further humanity's
                                         knowledge of how learning evolves by creating simulations and tools for 
                                         running simulations, and continue to improve
                                      """
                            , a [ href "https://github.com/julian-zucker" ] [ text "Evvo" ]
                            , text "."
                            ]
                        ]
                    ]
              ]
            )

        Essays key ->
            ( "Internalized blogphobia"
            , [ div []
                    ([ p [ class "essay-header" ] [ text "Essays are just pretentious blog posts." ] ]
                        ++ List.map Essay.viewEssayPreview Essay.essays
                    )
              ]
            )

        Log key ->
            ( "A dev log, not a web log", Log.viewLogs Log.weeklyLogs )

        LogEntry key model ->
            ( Log.pageTitle model, Log.view model )

        NoSuchLogEntry key ->
            ( "Lack of log"
            , [ div []
                    [ p [] [ text "The log page you were trying to access doesn't exist." ]
                    , p [] [ a [ href "/" ] [ text "Back to homepage." ] ]
                    ]
              ]
            )

        Resume key ->
            ( "Resume, as in unpause", Resume.view )



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
