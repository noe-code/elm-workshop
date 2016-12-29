module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)


type alias Result =
    { id : Int
    , name : String
    , stars : Int
    }


model : { result : Result }
model =
    { result =
        { id = 1
        , name = "TheSeamau5/elm-checkerboardgrid-tutorial"
        , stars = 66
        }
    }


viewHeader : Html a
viewHeader =
    header []
        [ h1 [] [ text "ElmHub" ]
        , span [ class "tagline" ] [ text "Like GitHub, but for Elm things." ]
        ]


viewResults : Html a
viewResults =
    ul [ class "results" ]
        [ li []
            [ span [ class "star-count" ]
                [ model.result.stars
                    |> toString
                    |> text
                ]
            , a [ href ("https://github.com/" ++ model.result.name) ] [ text model.result.name ]
            ]
        ]


main : Html a
main =
    div [ class "content" ]
        [ viewHeader
        , viewResults
        ]
