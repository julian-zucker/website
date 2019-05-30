module Route exposing (Route(..), fromUrl, toUrlString)

---- ROUTING ----

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = Home
    | Essays
    | Essay String
    | Reviews
    | Log
    | LogEntry Int
    | Resume


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Essays (s "essays")
        , Parser.map Essay (s "essay" </> Parser.string)
        , Parser.map Reviews (s "reviews")
        , Parser.map Resume (s "resume")
        , Parser.map Log (s "log")
        , Parser.map LogEntry (s "log_entry" </> Parser.int)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url


toUrlString : Route -> String
toUrlString route =
    case route of
        Home ->
            "/"

        Essays ->
            "/essays/"

        Essay slug ->
            "/essay/" ++ slug

        Reviews ->
            "/reviews/"

        Resume ->
            "/resume/"

        Log ->
            "/log/"

        LogEntry weekNum ->
            "/log/week/" ++ String.fromInt weekNum
