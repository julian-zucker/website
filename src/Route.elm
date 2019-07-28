module Route exposing (Route(..), fromUrl, fromUrlString, toUrlString)

---- ROUTING ----

import Url exposing (Url)
import Url.Builder
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = Home
    | Essays
    | Essay String
    | Log
    | LogEntry Int
    | Resume


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Essays (s "essays")
        , Parser.map Essay (s "essay" </> Parser.string)
        , Parser.map Resume (s "resume")
        , Parser.map Log (s "log")
        , Parser.map LogEntry (s "log" </> s "week" </> Parser.int)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


fromUrlString : String -> Maybe Route
fromUrlString urlString =
    Maybe.andThen
        (Parser.parse parser)
        (Url.fromString ("https://julianzucker.com" ++ Url.Builder.absolute [ urlString ] []))


toUrlString : Route -> String
toUrlString route =
    case route of
        Home ->
            "/"

        Essays ->
            "/essays/"

        Essay slug ->
            "/essay/" ++ slug

        Resume ->
            "/resume/"

        Log ->
            "/log/"

        LogEntry weekNum ->
            "/log/week/" ++ String.fromInt weekNum
