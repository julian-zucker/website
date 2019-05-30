module Review exposing (Model, reviews, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Url exposing (Url)


type alias Model =
    { name : String
    , link : Maybe Url
    , stars : NumberOfStars
    , review : String
    }


type NumberOfStars
    = OneStar
    | TwoStars
    | ThreeStars
    | FourStars
    | FiveStars


reviews : List Model
reviews =
    [ Model "IntelliJ/PyCharm/DataGrip/WebStorm"
        (Url.fromString "https://www.jetbrains.com")
        FiveStars
        "These are the best editors."
    , Model "Scala"
        (Url.fromString "https://www.scala-lang.org")
        FourStars
        """
        Worth learning, even if you go straight back to Java.
        It will show you the value of default immutable types,
        even in an OO language. A gateway drug to Haskell.
        """
    ]


view : List Model -> List (Html msg)
view reviewList =
    [ div [ class "review-list" ]
        (List.intersperse
            (hr [] [])
            (List.map viewSingleReview reviewList)
        )
    ]


viewSingleReview : Model -> Html msg
viewSingleReview model =
    div [ class "review-list-item" ]
        [ header []
            [ case model.link of
                Just url ->
                    a [ href (Url.toString url) ] [ text model.name ]

                Nothing ->
                    text model.name
            , viewStars model.stars
            ]
        , div [ class "review-list-item-text" ] [ text model.review ]
        ]


viewStars : NumberOfStars -> Html msg
viewStars n =
    let
        numFilled =
            case n of
                OneStar ->
                    1

                TwoStars ->
                    2

                ThreeStars ->
                    3

                FourStars ->
                    4

                FiveStars ->
                    5

        numUnfilled =
            5 - numFilled

        filledStar =
            "★"

        unfilledStar =
            "☆"
    in
    span [ class "review-item-stars" ]
        [ text (String.repeat numFilled filledStar ++ String.repeat numUnfilled unfilledStar) ]
