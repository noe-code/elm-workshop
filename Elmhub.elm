port module Elmhub exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, target, defaultValue)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (decodeString)
import Http
import Auth
import SearchOptions exposing (..)
import Table


-- TYPE ALIAS


type alias SearchResult =
    { id : Int
    , name : String
    , stars : Int
    }


type alias Model =
    { query : String
    , results : List SearchResult
    , errorMessage : Maybe String
    , searchOptions : SearchOptions.Model
    , tableState : Table.State
    }



-- MODEL


initialModel : Model
initialModel =
    { query = ""
    , results = []
    , errorMessage = Nothing
    , searchOptions = SearchOptions.initialModel
    , tableState = Table.initialSort "Stars"
    }


init : ( Model, Cmd a )
init =
    ( initialModel, Cmd.none )



-- DECODERS


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
    | SearchElm
    | SearchJS
    | HandleGithubResponse (Result Http.Error (List SearchResult))
    | HandleGithubResponseFromJS (Result String (List SearchResult))
    | SearchOptions SearchOptions.Msg
    | SetTableState Table.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DeleteById id ->
            ( { model | results = List.filter (\result -> result.id /= id) model.results }, Cmd.none )

        SetQuery str ->
            ( { model | query = str |> Debug.log "Debugging", errorMessage = Nothing }, Cmd.none )

        SearchElm ->
            ( { model | errorMessage = Nothing }, searchGithubApi (githubApiUrl model) )

        SearchJS ->
            ( { model | errorMessage = Nothing }, searchGithubApiWithJS (githubApiUrl model) )

        HandleGithubResponse (Ok results) ->
            ( { model | results = results }, Cmd.none )

        HandleGithubResponse (Err error) ->
            ( { model | errorMessage = Just (handleHttpErrorMessage error) }, Cmd.none )

        HandleGithubResponseFromJS (Ok results) ->
            ( { model | results = results }, Cmd.none )

        HandleGithubResponseFromJS (Err error) ->
            ( { model | errorMessage = Just error }, Cmd.none )

        SearchOptions searchOptionsMsg ->
            ( { model | searchOptions = SearchOptions.update searchOptionsMsg model.searchOptions }, Cmd.none )

        SetTableState newTableState ->
            ( { model | tableState = newTableState }, Cmd.none )


handleHttpErrorMessage : Http.Error -> String
handleHttpErrorMessage error =
    case error of
        Http.BadUrl url ->
            ("Invalid URL" ++ url)

        Http.NetworkError ->
            "Are sure the server is running?"

        Http.Timeout ->
            "Request time out"

        Http.BadStatus response ->
            case response.status.code of
                401 ->
                    "Unauthorized"

                404 ->
                    "Not found"

                code ->
                    "Error Code" ++ (toString code)

        Http.BadPayload msg _ ->
            "JSON Decoder error: " ++ msg



--PATERNS
-- "?key+value"
-- "&key=value"
-- "+key:value"


githubApiUrl : Model -> String
githubApiUrl model =
    let
        questionKeyEqualsValue key value =
            "?" ++ key ++ "=" ++ value

        ampersandKeyEqualsValue key value =
            "&" ++ key ++ "=" ++ value

        plusKeyColonValue key value =
            if String.isEmpty value then
                ""
            else
                "+" ++ key ++ ":" ++ value

        query =
            let
                minStars =
                    ">=" ++ (model.searchOptions.minStars |> toString)
            in
                model.query
                    ++ plusKeyColonValue "in" model.searchOptions.searchIn
                    ++ plusKeyColonValue "stars" minStars
                    ++ plusKeyColonValue "user" model.searchOptions.userFilter
                    ++ plusKeyColonValue "language" "elm"
    in
        "https://api.github.com/search/repositories"
            ++ questionKeyEqualsValue "access_token" Auth.token
            ++ ampersandKeyEqualsValue "q" query



-- COMMANDS


searchGithubApi : String -> Cmd Msg
searchGithubApi query =
    let
        getRequest =
            Http.get query githubDecoder
    in
        Http.send HandleGithubResponse getRequest


port searchGithubApiWithJS : String -> Cmd a



-- SUBSCRIPTIONS


port responseFromGithubApiWithJS : (Json.Decode.Value -> msg) -> Sub msg


decodeResponseFromJS : Json.Decode.Value -> Msg
decodeResponseFromJS json =
    HandleGithubResponseFromJS (Json.Decode.decodeValue githubDecoder json)


subscriptions : a -> Sub Msg
subscriptions =
    (\_ -> responseFromGithubApiWithJS decodeResponseFromJS)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader
        , viewSearch model
        , viewErrorMessage model.errorMessage
        , viewResults model.tableState model.results
        ]


viewHeader : Html a
viewHeader =
    header []
        [ h1 [] [ text "ElmHub" ]
        , span [ class "tagline" ] [ text "Like GitHub, but for Elm things." ]
        ]


viewSearch : Model -> Html Msg
viewSearch model =
    div [ class "search" ]
        [ Html.map SearchOptions (SearchOptions.view model.searchOptions)
        , viewSearchInput model.query
        ]


viewSearchInput : String -> Html Msg
viewSearchInput query =
    div [ class "search-input" ]
        [ input [ class "search-query", onInput SetQuery, defaultValue query ] []
        , button [ class "search-button", onClick SearchElm ] [ text "Search Elm" ]
        , button [ class "search-button", onClick SearchJS ] [ text "Search JS" ]
        ]


viewErrorMessage : Maybe String -> Html a
viewErrorMessage message =
    case message of
        Just msg ->
            div [ class "error" ] [ text msg ]

        Nothing ->
            div [] [ text "" ]


viewResults : Table.State -> List SearchResult -> Html Msg
viewResults currentTableState results =
    let
        tableConfig =
            Table.config
                { toId = .id >> toString
                , toMsg = SetTableState
                , columns =
                    [ starsCustomColumn
                    , nameCustomColumn
                    ]
                }
    in
        Table.view tableConfig currentTableState results


nameCustomColumn : Table.Column SearchResult Msg
nameCustomColumn =
    Table.veryCustomColumn
        { name = "Name"
        , viewData = viewNameColumn
        , sorter = Table.decreasingOrIncreasingBy .name
        }


viewNameColumn : SearchResult -> Table.HtmlDetails Msg
viewNameColumn searchResult =
    Table.HtmlDetails []
        [ a [ href ("https://github.com/" ++ searchResult.name), target "_blank" ] [ text searchResult.name ]
        , button [ class "hide-result", onClick (DeleteById searchResult.id) ] [ text "X" ]
        ]


starsCustomColumn : Table.Column SearchResult Msg
starsCustomColumn =
    Table.veryCustomColumn
        { name = "Stars"
        , viewData = viewStarsColumn
        , sorter = Table.decreasingOrIncreasingBy .stars
        }


viewStarsColumn : SearchResult -> Table.HtmlDetails Msg
viewStarsColumn searchResult =
    Table.HtmlDetails []
        [ span [ class "star-count" ] [ searchResult.stars |> toString |> text ] ]
