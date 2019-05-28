module Route exposing (Route(..), fromUrl)

---- ROUTING ----

import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s)


type Route
    = Home
    | BlogHome
    | Blog String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map BlogHome (s "blog")
        , Parser.map Blog (s "blog" </> Parser.string)
        ]


fromUrl : Url -> Maybe Route
fromUrl url =
    Parser.parse parser url
