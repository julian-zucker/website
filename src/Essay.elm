module Essay exposing (Model, essays, getEssayByName, view, viewEssayLink, viewEssayPreview)

import Html exposing (..)
import Html.Attributes exposing (..)
import Route
import Url



{-
   This module has some problems.
   * Essays can only be plaintext, no formatting or even links.
   * The source of every essay has to be in code in this file. I want to store the text in plaintext files
   that I read at build time.
   * Essays don't have slugs, only the full name. Duplicate names aren't an issue (I doubt I'll publish two files with
    the same name) but the URLs are ugly.
-}


type alias Model =
    { name : String
    , content : List String
    }


essays : List Model
essays =
    -- written 2019-06-02
    [ Model "Deliberate Practice for Software Engineering"
        [ """Solving HackerRank-style programming challenges will not make you a better software developer.
        """
        , """Day-to-day software engineering almost never requires implementation of algorithms, and when it does, it is important that your implementations are maintainable. Practicing how to write one-off algorithms then will not help you practice software engineering. It may give you skills that will be helpful in software engineering – learning the full API of the standard libraries of languages, practicing writing some types of code fast, and general debugging. But it won't help you as much as deliberate practice for software engineering.
        """
        , """What would a programming challenge intended to be delibrate practice look like? I don't know exactly, and to figure it out we first have to define software engineering, so that we can see what it means to get better at it. Let's say software engineering is writing maintainable, correct code. If someone writes more maintainable code than they used to, we would say they're a better software engineer, and the same for writing code that is closer to correct. HackerRank-style challenges will help with correctness, as your code does have to pass a suite of unit tests. But it doesn't help you with _ensuring_ that your code is closer to correct, because you aren't writing your own unit tests. If you have someone else's test harness to lean on, you aren't practicing the skill of testing, which means a programming challenge where you have to write your own tests would be superior. HackerRank-style challenges also don't reward maintainability, in fact, by only judging competitors on speed to solve the problem, writing code that is maintainable will hurt your performance.
        """
        , """How can we test programmer's on their ability to test? One way of doing this is asking programmers to write unit tests for a problem without writing code, and then seeing if their tests pass with correct implementations and fail on incorrect implementations. This has some issues, most notably that typos in the unit tests would have a huge effect on outcomes, and that the iterative write-test/write-code/debug cycle is the skill we really care about, not writing the tests correctly on the first try. Perhaps it would be better to get developers to     write code and tests, or just make the code complex enough that automated unit tests are useful.
        """
        , """We can test maintainability of programs by having programmers maintain the programs they write for the coding challenge. We can't simulate the experience of opening up a file you haven't touched in a year and having no clue what's happening in it unless we are willing to wait a year. But waiting a year gets rid of the tight feedback loops that you need for deliberate practice. So, we will have people maintain their programs immediately. First, give your programmers a small task, have them implement it, and run their code past your unit tests. Score their solution on correctness on the first challenge, and then have them update the code, adding some features and changing some functionality. Repeat a few more times. Now, you can see how quickly a programmer was able to write and modify their code to meet changing requirements, as well as the functionality along each step of the way.
        """
        , """If you're using this tool for deliberate practice, you would be able to see which of your decisions lead to maintainable code and which were difficult to update. This would allow you to practice writing maintainable code, something which current programming challenges don't allow. I appreciate that Object-Oriented Design at Northeastern does something similar. For the final two months of the class, you iteratively design a model, view, and controller for an animation that users can dynamically interact with. They manually grade test coverage for each assignment, but without tests, I doubt people would be able to make the third or fourth set of changes without regressions on the first set of requirements. 
        """
        , """Modifying code, not writing code, is most of software engineering, so our classes, challenges, and practice tools should focus on modifying code.
        """
        ]
    ]


getEssayByName : String -> Maybe Model
getEssayByName essayName =
    List.head (List.filter (\p -> Just p.name == Url.percentDecode essayName) essays)


view : Model -> ( String, List (Html msg) )
view { name, content } =
    ( name
    , [ div [ class "blog-content" ]
            [ div [ class "blog-text" ]
                (List.map (\c -> p [] [ text c ]) content)
            ]
      ]
    )


viewEssayPreview : Model -> Html msg
viewEssayPreview model =
    div []
        [ p [] [ viewEssayLink model.name ]
        , p [ class "essay-preview" ] [ text (String.left 160 (List.foldr (++) "" model.content) ++ "…") ]
        ]


viewEssayLink : String -> Html msg
viewEssayLink link =
    span [ class "essay-link" ] [ a [ href (Route.toUrlString (Route.Essay link)) ] [ text link ] ]
