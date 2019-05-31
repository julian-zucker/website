module Log exposing (Model, getLogForWeek, pageTitle, view, viewLogs, weeklyLogs)

-- TODO implement weekly log

import Html exposing (..)
import Html.Attributes exposing (href)
import Route


type alias Model =
    { week : Int
    , logItems : List String
    }


getLogForWeek : Int -> Maybe Model
getLogForWeek weekNum =
    -- Returns the nth item of the list. 1-indexed
    List.head (List.filter (\log -> log.week == weekNum) weeklyLogs)


weeklyLogs : List Model
weeklyLogs =
    [ Model 1
        [ "Created this website!"
        , "Dockerized Evvo, and documented how to create/build/deploy/run the docker image on GCP instances"
        , "Deployed Evvo to Multiple GCP instances, tested clustering"
        , "Added methods to BallotBoxes in my social choice library to enable ordering-based "
            ++ "methods on pairwise comparison vote sets. This one is bigger than it sounds, "
            ++ "I think it may be a novel contribution to the field. Have to do more research here."
        , "Implemented Borda Count for my social choice library."
        , "Replicated findings in \"The Emergence of Intersectional Disadvantage\" by recreating"
            ++ " their model of Moderate intersectionality and running it on a GCP server with 16 cores. "
            ++ " Renting this server costs $1/hour. I love cloud computing."
        , "Configured zsh: autocompletions, transferred my bash aliases over, installed plugins."
        ]
    ]


viewLogs : List Model -> List (Html msg)
viewLogs logs =
    List.map viewSummary (List.reverse (List.sortBy .week logs))


viewSummary : Model -> Html msg
viewSummary model =
    div []
        [ a [ href (Route.toUrlString (Route.LogEntry model.week)) ]
            -- TODO should render the actual dates (week 1 is the week of Monday, May 27, 2019
            [ text ("Log: Week " ++ String.fromInt model.week)
            ]
        ]


view : Model -> List (Html msg)
view model =
    let
        viewLogItem : String -> Html msg
        viewLogItem item =
            li [] [ text item ]
    in
    [ div []
        [ span [] [ text ("Dev Log: Week " ++ String.fromInt model.week) ]
        , ul [] (List.map viewLogItem model.logItems)

        -- TODO: should only render as link if something actually exists there
        , div [] [ a [ href (Route.toUrlString (Route.LogEntry (model.week - 1))) ] [ text "Previous week" ] ]
        , div [] [ a [ href (Route.toUrlString (Route.LogEntry (model.week + 1))) ] [ text "Next week" ] ]
        ]
    ]


pageTitle : Model -> String
pageTitle model =
    "Dev Log Week " ++ String.fromInt model.week
