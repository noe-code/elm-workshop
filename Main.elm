module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, target)
import Html.Events exposing (onClick)


type alias SearchResult =
    { id : Int
    , name : String
    , stars : Int
    }


type alias Model =
    { query : String
    , results : List SearchResult
    }



-- MODEL


initialModel : Model
initialModel =
    { query = ""
    , results =
        [ { id = 1
          , name = "TheSeamau5/elm-checkerboardgrid-tutorial"
          , stars = 66
          }
        , { id = 2
          , name = "grzegorzbalcerek/elm-by-example"
          , stars = 41
          }
        , { id = 3
          , name = "sporto/elm-tutorial-app"
          , stars = 35
          }
        , { id = 4
          , name = "jvoigtlaender/Elm-Tutorium"
          , stars = 10
          }
        , { id = 5
          , name = "sporto/elm-tutorial-assets"
          , stars = 7
          }
        ]
    }



-- UPDATE


type Msg
    = DeleteById Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        DeleteById id ->
            { model | results = List.filter (\result -> result.id /= id) model.results }



-- VIEW


viewHeader : Html a
viewHeader =
    header []
        [ h1 [] [ text "ElmHub" ]
        , span [ class "tagline" ] [ text "Like GitHub, but for Elm things." ]
        ]


viewResults : List SearchResult -> Html Msg
viewResults results =
    ul [ class "results" ] (List.map viewSearchResults results)


viewSearchResults : SearchResult -> Html Msg
viewSearchResults result =
    li []
        [ span [ class "star-count" ]
            [ result.stars
                |> toString
                |> text
            ]
        , a [ href ("https://github.com/" ++ result.name), target "_blank" ] [ text result.name ]
        , button [ class "hide-result", onClick (DeleteById result.id) ] [ text "X" ]
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader
        , viewResults model.results
        ]



-- MAIN


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
