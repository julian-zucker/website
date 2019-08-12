module Log exposing (Model, getLogForWeek, pageTitle, view, viewLogs, weeklyLogs)

-- TODO implement weekly log

import Date exposing (Date, Interval(..), Unit(..))
import Html exposing (..)
import Html.Attributes exposing (href)
import Route
import Time exposing (Month(..))


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
        , "Dockerized Evvo, and documented how to create/build/deploy/run the docker image on GCP instances."
        , "Deployed Evvo to Multiple GCP instances, tested clustering."
        , "Added methods to BallotBoxes in my social choice library to enable ordering-based methods on pairwise comparison vote sets. This one is bigger than it sounds, I think it may be a novel contribution to the field. Have to do more research here."
        , "Implemented Borda Count for my social choice library."
        , "Replicated findings in \"The Emergence of Intersectional Disadvantage\" by recreatingtheir model of Moderate intersectionality and running it on a GCP server with 16 cores. Renting this server costs $1/hour. I love cloud computing."
        , "Configured zsh: autocompletions, transferred my bash aliases over, installed plugins."
        ]
    , Model 2
        [ """Learned how to use SLURM, used it for batch processing of simulations - 1000 cores over 100 machines!"""
        , """Found performance improvements for simulations (essentially memoization) – roughly square rooting computation time. Still takes a while though! """
        , "Registered evvo.io, redirected to github page."
        , "Registered for an account on the Sonatype maven repository, prepared to upload v0.0.0 of Evvo."
        , "Wrote slides and sample code for week 2 of Machine Learning Workshop, focusing on ensemble methods."
        , """Investigated some ways to seriarlize scala.Function0's. It is quite hard under the default Java serializer, as it casts to a java.lang.invoke.SerializedLambda, which then throws an error when being cast into a scala.Function0. This is preventing complex functions from being used in Evvo."""
        , "Added emigration to Evvo, improving performance when running in parallel."
        , "Read through some old code of Evvo, finding refactors that should be done and recording them in Github issues."
        ]
    , Model 3
        [ """Finally got Evvo to correctly serialize/deserialize complex behavior for agents."""
        , """Including writing a test for serializability, which lead me to read a lot about Java serialization/protobuf."""
        , """Did a refactor in Evvo for end user convenience, and to make names consistent with the EC literature."""
        , """Read through the logback docs to figure out why Evvo wasn't logging anything, patched."""
        , """Rewrote evolutionary game theory simulation code to leverage implicits to avoid the need for global variables."""
        , """Rewrote evolutionary game theory simulation code to clean up strange design that happened as a result of time crunch."""
        , """Wrote tests for every class in evolutionary game theory simulation code."""
        , """Factored out common result type for EGT simulations, separating data processing logic from visualizations."""
        ]
    , Model 4
        [ """Wrote new simulations for exploring multiple groups interacting, including new type of model (as far as I know, completely novel) where group size changes over time."""
        , """Updated old simulations to test the effect of precedent - how does the initial strategy distribution affect which strategies the groups converge to? """
        , """Implemented the Push language in Scala, to prepare for parallelizing PushGP with Evvo."""
        , """Implemented "Immigration Strategies" for Evvo, so islands can choose which immigrants to accept."""
        , """Reviewed the Evvo codebase, adding missing docs and tests."""
        , """Read the git-scm book. I knew most of what was in there, especially how to use the tools like branching/committing/unstaging files, but having someone explain it explicitly gave me a better conceptual understanding of what I was actually doing, and why. And I feel more comfortable with `rebase` now."""
        , """I didn't get as much done this week as I wanted, but I was busy saying my goodbyes and packing, as I'm leaving for Denver on Monday! I'm going to call that an acceptable excuse."""
        ]
    , Model 5
        [ """Made it to Denver, unpacked. I spent most of the week without a desk, so most of my work was done in coffee shops, and I missed my ErgoDox. Denver has good coffee though!"""
        , """Worked on an implementation of the Push language in Scala, though not quite done yet. The goal is to parallelize genetic programming with Push using Evvo, which will help with program induction research and also create a publishable unit for Evvo."""
        , """Related to reading the git-scm book last week, felt much more confident with merges/rebases this week."""
        , """Finished the socialchoice project. Parallelized using SLURM, ran the results, wrote up the paper. I'm going to circulate the draft to some professors, get some feedback, and try to publish it."""
        , """Finished the quickstart guide for Evvo. It should be done now, I think we can start publicizing."""
        , """Updated Evvo to Scala 2.13, which I'm excited for. I've been waiting for `.pipe` for a while."""
        , """Designed and wrote immigration/emigration strategies. Now, end users of Evvo have control over their island's communication. Although, not as much as there should be. For example, no control over how often the emigration happens, or what the network topology is. I'll create some tickets for it next week."""
        ]
    , Model 6
        [ "Started working at Pivotal! This reduced the number of free hours I have by ~40/week."
        , """Published Evvo on Maven, requiring a hefty time investment in updating xml files and reading documentation.
        But it's worth it, because if you're writing code for people to use, you need to make it easy to use."""
        , """Dug through the docs for the Java Executors, and used this to fix some stubbornly still-serial code in Evvo that by all indicatinos should have been running in parallel."""
        , "Implemented binary trees, for maybe my seventh time, in order to provide them as a built-in datatype in Evvo."
        , "Used those binary trees to start the \"Evolving Fair Models \" project."
        ]
    , Model 7
        [ "This week felt unproductive, but we'll see what the commit logs say below."
        , """At Pivotal, I went through a 1-day training in k8s, and have been working on an extensive refactor to reduce duplication in the internal representations of indicator document's data. """
        , """Wrote scripts to download and parse the German credit dataset for Evolving Fair Models (EFM)."""
        , """Evolved neural-net-competitive decision trees on the German credit dataset. using FP and FN rate as two objectives."""
        , """Created an objective for fairness in EFM, trained models against that metric."""
        , """Minor cleanups and refactors in Evvo, added a new format to Pareto frontiers where the solutions are sorted by one objective, making it easier to compare."""
        , """Looking at this list, I did more than I thought, but less than I would have liked. So it goes. There's always next week!"""
        ]
    , Model 8
        [ "Learned a bit of LaTeX, started working on my CV for PhD applications."
        , "Improved this website significantly, little usability improvements like a better 404 page and not saying \"next log\" if there isn't one."
        , "Wrote a `jq` one-liner for my PM at pivotal, so that he could look at only the relevant bits of some API responses. I knew ankifying `jq` would pay off!"
        , "Made a PR to fix a typo in Go By Example. It was slightly nerve-wracking to contribute to someone else's OSS project, even though it's a very kind community."
        , "Set up my keychain thumb drive with a script that loads my git ssh key, so that I can push code from anywhere."
        , "Evolved fair decision trees with accuracy similar to benchmarks in the literature! This is really exciting to me, and I have some awesome visualizations showing the cost of fairness on accuracy. Paper forthcoming."
        ]
    , Model 9
        [ "Set up the evolving fair models code to be more reproducible. Produced visualizations of the output."
        , "Updated some of the code for this website: logs now have their week, there's a link to my resume, etc."
        , "Added customizable network topology to the islands in Evvo. This is the last piece of core functionality that was hardcoded, everything is now fully pluggable."
        , "More LaTeX nonsense as I work on migrating the \"Evolving Fair Models\" paper to Google Docs."
        ]
    , Model 10
        [ "Working on papers, lots of writing and creating figures."
        , "Trying to get Evvo running on k8s. Looks like it might require swapping from Akka Remoting to Akka HTTP."
        ]
    , Model 11
        [ "At Pivotal, I wrestled with UAA, BOSH, etc. I really appreciate the powerful tools that Pivotal uses, but learning how to use them and how they all fit together will take some work."
        , "Minor bugfixes in Evvo."
        , "Changed some APIs in Evvo to be more consistent. If you have multiple implementations of a trait, some of which are case objects and some of which are case classes, you will get confused."
        , "Wrestled with moving Evvo to Kubernetes, and failed because Akka Remoting doesn't play nicely with other networking systems. I will try to update that"
        , "Updated how graphs were generated for \"Evolving Fair Models\", to automate the removal of whitespace that I was doing by hand."
        , "LaTeXified the entire EFM paper, and also started work on evolutionary game theory with dynamic group sizes."
        , "Updated essays on this website to support block quotes."
        ]
    ]


viewLogs : List Model -> List (Html msg)
viewLogs logs =
    List.map viewSummary (List.reverse (List.sortBy .week logs))



-- Returns the string of the monday of the week of the model, assuming week number is
-- weeks since May 27 2019.


weekString : Model -> String
weekString model =
    Date.fromCalendarDate 2019 May 27
        |> Date.add Weeks (model.week - 1)
        |> Date.format "MMMM ddd, yyyy"


viewSummary : Model -> Html msg
viewSummary model =
    div []
        [ a [ href (Route.toUrlString (Route.LogEntry model.week)) ]
            [ text ("Week of " ++ weekString model) ]
        ]


view : Model -> List (Html msg)
view model =
    let
        viewLogItem : String -> Html msg
        viewLogItem item =
            li [] [ text item ]
    in
    [ div []
        ([ span [] [ text ("Week of " ++ weekString model) ]
         , ul [] (List.map viewLogItem model.logItems)
         ]
            ++ viewLogLink (model.week + 1) "Next week"
            ++ viewLogLink (model.week - 1) "Previous week"
        )
    ]



-- Renders a link to log for week `index`, with message `description`, if such a model exists,
-- otherwise returns the empty list.


viewLogLink : Int -> String -> List (Html msg)
viewLogLink index description =
    case getLogForWeek index of
        Just _ ->
            [ div [] [ a [ href (Route.toUrlString (Route.LogEntry index)) ] [ text description ] ] ]

        Nothing ->
            []


pageTitle : Model -> String
pageTitle model =
    "Dev Log Week " ++ String.fromInt model.week
