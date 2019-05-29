module Route exposing (Route(..), fromUrl, toUrlString)

---- ROUTING ----

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = Home
    | Essays
    | Essay String
    | Reviews
    | Links


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Essays (s "essays")
        , Parser.map Essay (s "essay" </> Parser.string)
        , Parser.map Reviews (s "reviews")
        , Parser.map Links (s "links")
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
            "/essays/" ++ slug

        Reviews ->
            "/reviews/"

        Links ->
            "/links/"
