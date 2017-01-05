module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, target, defaultValue)
import Html.Events exposing (onClick, onInput)
import FakeResponse
import Json.Decode exposing (decodeString)


-- TYPE ALIAS


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
    { query = "Tutorial"
    , results = fakeResuts
    }



-- DECODERS


fakeResuts : List SearchResult
fakeResuts =
    decodeResults FakeResponse.json


decodeResults : String -> List SearchResult
decodeResults json =
    case (decodeString githubDecoder json) of
        Ok searchResults ->
            searchResults

        Err error ->
            let
                _ =
                    Debug.log "el error es" error
            in
                []


githubDecoder : Json.Decode.Decoder (List SearchResult)
githubDecoder =
    Json.Decode.at [ "items" ]
        (Json.Decode.list
            (Json.Decode.map3 SearchResult
                (Json.Decode.field "id" Json.Decode.int)
                (Json.Decode.field "full_name" Json.Decode.string)
                (Json.Decode.field "stargazers_count" Json.Decode.int)
            )
        )



-- UPDATE


type Msg
    = DeleteById Int
    | SetQuery String


update : Msg -> Model -> Model
update msg model =
    case msg of
        DeleteById id ->
            { model | results = List.filter (\result -> result.id /= id) model.results }

        SetQuery str ->
            { model | query = str |> Debug.log "Debugging" }



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader
        , viewSearch model.query
        , viewResults model.results
        ]


viewHeader : Html a
viewHeader =
    header []
        [ h1 [] [ text "ElmHub" ]
        , span [ class "tagline" ] [ text "Like GitHub, but for Elm things." ]
        ]


viewSearch : String -> Html Msg
viewSearch query =
    div []
        [ input [ class "search-query", onInput SetQuery, defaultValue query ] []
        , button [ class "search-button" ] [ text "Search" ]
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



-- MAIN


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initialModel
        , view = view
        , update = update
        }
