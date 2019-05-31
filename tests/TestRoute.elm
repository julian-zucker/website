module TestRoute exposing (all, urlToRouteTests)

import Expect
import Route
import Test exposing (..)
import Url



-- Check out http://package.elm-lang.org/packages/elm-community/elm-test/latest to learn more about testing in Elm!


all : Test
all =
    describe "Parsing routes from urls"
        [ test "Parsing log entry routes" <|
            \_ ->
                Expect.equal (Just (Route.LogEntry 2)) (Route.fromUrlString "log/week/2")
        , test "Parsing log route" <|
            \_ ->
                Expect.equal (Just Route.Log) (Route.fromUrlString "log")
        , test "Parsing top" <|
            \_ ->
                Expect.equal (Just Route.Home) (Route.fromUrlString "/")
        ]


urlToRouteTests : Test
urlToRouteTests =
    describe "Generating urls from routes"
        [ test "Parsing log entry routes" <|
            \_ ->
                Expect.equal "/log/week/2" (Route.toUrlString (Route.LogEntry 2))
        , test "Parsing log route" <|
            \_ ->
                Expect.equal "/log/" (Route.toUrlString Route.Log)
        , test "Parsing top" <|
            \_ ->
                Expect.equal "/" (Route.toUrlString Route.Home)
        ]
