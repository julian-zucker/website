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
    , Model 2
        [ """Learned how to use SLURM, used it for batch processing of simulations - 1000 cores over 100 machines!"""
        , """Found performance improvements for simulations (essentially memoization) 
             – roughly square rooting computation time. Still takes a while though!
             """
        , "Registered evvo.io, redirected to github page."
        , "Registered for an account on the Sonatype maven repository, prepared to upload v0.0.0 of Evvo."
        , "Wrote slides and sample code for week 2 of Machine Learning Workshop, focusing on ensemble methods."
        , """Investigated some ways to seriarlize scala.Function0's. It is quite hard under the default Java serializer
             , as it casts to a java.lang.invoke.SerializedLambda, which then throws an error when being cast 
             into a scala.Function0. This is preventing complex functions from being used in Evvo."""
        , "Added emigration to Evvo, improving performance when running in parallel."
        , "Read through some old code of Evvo, finding refactors that should be done and recording them in Github issues."
        ]
    , Model 3
        [ """Finally got Evvo to correctly serialize/deserialize complex behavior for agents."""
        , """Including writing a test for serializability, which lead me to read a lot about Java serialization/protobuf."""
        , """Did a refactor in Evvo for end user convenience, and to make names consistent with the EC literature."""
        , """Read through the logback docs to figure out why Evvo wasn't logging anything, patched."""
        , """Rewrote evolutionary game theory simulation code to leverage implicits to avoid
         the need for global variables."""
        , """Rewrote evolutionary game theory simulation code to clean up strange design that happened as a
         result of time crunch."""
        , """Wrote tests for every class in evolutionary game theory simulation code."""
        , """Factored out common result type for EGT simulations, separating data processing logic from visualizations."""
        ]
    , Model 4
        [ """Wrote new simulations for exploring multiple groups interacting, including new type of model (as far as I
        know, completely novel) where group size changes over time."""
        , """Updated old simulations to test the effect of precedent - how does the initial strategy distribution affect
        which strategies the groups converge to? """
        , """Implemented the Push language in Scala, to prepare for parallelizing PushGP with Evvo."""
        , """Implemented "Immigration Strategies" for Evvo, so islands can choose which immigrants to accept."""
        , """Reviewed the Evvo codebase, adding missing docs and tests."""
        , """Read the git-scm book. I knew most of what was in there, especially how to use the tools like branching/
        committing/unstaging files, but having someone explain it explicitly gave me a better conceptual understanding
        of what I was actually doing, and why. And I feel more comfortable with `rebase` now."""
        , """I didn't get as much done this week as I wanted, but I was busy saying my goodbyes and packing, as I'm
        leaving for Denver on Monday! I'm going to call that an acceptable excuse."""
        ]
    , Model 5
        [ """Made it to Denver, unpacked. I spent most of the week without a desk, so most of my work was done
        in coffee shops, and I missed my ErgoDox. Denver has good coffee though!"""
        , """Worked on an implementation of the Push language in Scala, though not quite done yet. The goal is to
         parallelize genetic programming with Push using Evvo, which will help with program induction research
         and also create a publishable unit for Evvo."""
        , """Related to reading the git-scm book last week, felt much more confident with merges/rebases this week."""
        , """Finished the socialchoice project. Parallelized using SLURM, ran the results, wrote up the paper. I'm
        going to circulate the draft to some professors, get some feedback, and try to publish it."""
        , """Finished the quickstart guide for Evvo. It should be done now, I think we can start publicizing."""
        , """Updated Evvo to Scala 2.13, which I'm excited for. I've been waiting for `.pipe` for a while."""
        , """Designed and wrote immigration/emigration strategies. Now, end users of Evvo have control over their
        island's communication. Although, not as much as there should be. For example, no control over how often
         the emigration happens, or what the network topology is. I'll create some tickets for it next week."""
        ]
    , Model 6
        [ "Started working at Pivotal! This reduced the number of free hours I have by ~40/week."
        , """Published Evvo on Maven, requiring a hefty time investment in updating xml files and reading documentation.
        But it's worth it, because if you're writing code for people to use, you need to make it easy to use."""
        , """Dug through the docs for the Java Executors, and used this to fix some stubbornly still-serial code
        in Evvo that by all indicatinos should have been running in parallel."""
        , "Implemented binary trees, for maybe my seventh time, in order to provide them as a built-in datatype in Evvo."
        , "Used those binary trees to start the \"Evolving Fair Models \" project."
        ]
    ]


viewLogs : List Model -> List (Html msg)
viewLogs logs =
    List.map viewSummary (List.reverse (List.sortBy .week logs))


viewSummary : Model -> Html msg
viewSummary model =
    div []
        [ a [ href (Route.toUrlString (Route.LogEntry model.week)) ]
            -- TODO should render the actual dates (week 1 is the week of Monday, May 27, 2019)
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
