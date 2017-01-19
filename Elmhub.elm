port module Elmhub exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, target, defaultValue, type_, value, selected, placeholder)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (decodeString)
import Http
import Auth


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
    , searchOptions : SearchOptionsModel
    }


type alias SearchOptionsModel =
    { minStars : Int
    , minStarsError : Maybe String
    , searchIn : String
    , userFilter : String
    }



-- MODEL


initialModel : Model
initialModel =
    { query = ""
    , results = []
    , errorMessage = Nothing
    , searchOptions =
        { minStars = 5
        , minStarsError = Nothing
        , searchIn = "name"
        , userFilter = ""
        }
    }


searchOptionsInitialModel : SearchOptionsModel
searchOptionsInitialModel =
    { minStars = 5
    , minStarsError = Nothing
    , searchIn = "name"
    , userFilter = ""
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
    | SearchOptions SearchOptionsMsg


type SearchOptionsMsg
    = SetMinStars String
    | SetSearchIn String
    | SetUserFilter String


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
            ( { model | searchOptions = updateSearchOptions searchOptionsMsg model.searchOptions }, Cmd.none )


updateSearchOptions : SearchOptionsMsg -> SearchOptionsModel -> SearchOptionsModel
updateSearchOptions searchOptionsMsg searchOptionsModel =
    case searchOptionsMsg of
        SetMinStars str ->
            case (String.toInt str) of
                Ok minStars ->
                    { searchOptionsModel | minStars = minStars, minStarsError = Nothing }

                Err err ->
                    { searchOptionsModel | minStarsError = Just "Must be a number!" }

        SetSearchIn searchIn ->
            { searchOptionsModel | searchIn = searchIn }

        SetUserFilter userFilter ->
            { searchOptionsModel | userFilter = userFilter }


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


githubApiUrl : Model -> String
githubApiUrl model =
    "https://api.github.com/search/repositories?access_token="
        ++ Auth.token
        ++ "&q="
        ++ model.query
        ++ "+in:"
        ++ model.searchOptions.searchIn
        ++ "+stars:>="
        ++ (model.searchOptions.minStars |> toString)
        ++ (if String.isEmpty model.searchOptions.userFilter then
                ""
            else
                "+user:" ++ model.searchOptions.userFilter
           )
        ++ "+language:elm&sort=stars&order=desc"



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



-- CUSTOM EVENT HANDLERS


onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue toMsg =
    Html.Events.on "blur" (Json.Decode.map toMsg Html.Events.targetValue)


onChange : (String -> msg) -> Attribute msg
onChange toMsg =
    Html.Events.on "change" (Json.Decode.map toMsg Html.Events.targetValue)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ viewHeader
        , viewSearch model
        , viewErrorMessage model.errorMessage
        , viewResults model.results
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
        [ Html.map SearchOptions (viewSearchOptions model.searchOptions)
        , viewSearchInput model.query
        ]


viewSearchInput : String -> Html Msg
viewSearchInput query =
    div [ class "search-input" ]
        [ input [ class "search-query", onInput SetQuery, defaultValue query ] []
        , button [ class "search-button", onClick SearchElm ] [ text "Search Elm" ]
        , button [ class "search-button", onClick SearchJS ] [ text "Search JS" ]
        ]



-- Beging view search options


viewSearchOptions : SearchOptionsModel -> Html SearchOptionsMsg
viewSearchOptions searchOptionsModel =
    div [ class "search-options" ]
        [ viewMinStars searchOptionsModel.minStars searchOptionsModel.minStarsError
        , viewUserFilter searchOptionsModel.userFilter
        , viewSearchIn
        ]


viewMinStars : Int -> Maybe String -> Html SearchOptionsMsg
viewMinStars minStars minStarsError =
    div [ class "search-option" ]
        [ viewMinStarsInput minStars
        , viewMinStarsError minStarsError
        ]


viewMinStarsInput : Int -> Html SearchOptionsMsg
viewMinStarsInput minStars =
    div []
        [ label [ class "top-label" ] [ text "Minimun Stars" ]
        , input
            [ type_ "text"
            , onBlurWithTargetValue SetMinStars
            , defaultValue (toString minStars)
            ]
            []
        ]


viewMinStarsError : Maybe String -> Html a
viewMinStarsError minStarsError =
    case minStarsError of
        Just error ->
            div [ class "stars-error" ] [ text error ]

        Nothing ->
            div [] [ text "" ]


viewUserFilter : String -> Html SearchOptionsMsg
viewUserFilter userFilter =
    div [ class "search-option" ]
        [ label [ class "top-label" ] [ text "Owned by" ]
        , input [ type_ "text", onInput SetUserFilter, defaultValue userFilter, placeholder "Github Username" ] []
        ]


viewSearchIn : Html SearchOptionsMsg
viewSearchIn =
    div [ class "search-option" ]
        [ label [ class "top-label" ] [ text "Search In" ]
        , select [ onChange SetSearchIn ]
            [ option [ value "name", selected True ] [ text "Name" ]
            , option [ value "description" ] [ text "Description" ]
            , option [ value "name,description" ] [ text "Name & Description" ]
            ]
        ]



-- End view search options


viewErrorMessage : Maybe String -> Html a
viewErrorMessage message =
    case message of
        Just msg ->
            div [ class "error" ] [ text msg ]

        Nothing ->
            div [] [ text "" ]


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
