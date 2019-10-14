module TwelveProblems exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)


view : List (Html msg)
view =
    [ div []
        [ div [ style "padding-bottom" ".6em" ] [ text """Gian-Carlo Rota writes: """ ]
        , div [ style "padding-bottom" ".6em", style "padding-left" "1.8em" ]
            [ span [] [ text """Richard Feynman was fond of giving the following advice on how to be a genius. You have to keep a dozen of your favorite problems constantly present in your mind, although by and large they will lay in a dormant state. Every time you hear or read a new trick or a new result, test it against each of your twelve problems to see whether it helps. Every once in a while there will be a hit, and people will say: “How did he do it? He must be a genius!”""" ] ]
        , div [] [ span [] [ text """So, here are mine:""" ] ]
        ]
    , div []
        [ ul []
            [ li [] [ text "How can we use natural-selection-like processes in computing?" ]
            , li [] [ text "How did interpersonal norms such as morality evolve?" ]
            , li [] [ text "How did the ability to learn evolve?" ]
            , li [] [ text "What can computational models tell us about philosophical problems?" ]
            , li [] [ text "What is the fundamental nature of an algorithm?" ]
            , li [] [ text "How can we practice ethical machine learning?" ]
            , li [] [ text "How can we best teach computer science?" ]
            , li [] [ text "How can we solve multi-objective optimization problems?" ]
            , li [] [ text "Why do people have akrasia?" ]
            , li [] [ text "..." ]
            , li [] [ text "... Looks like I only have ten." ]
            ]
        ]
    ]
