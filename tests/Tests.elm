module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int)
import String
import Json.Decode
import Elmhub exposing (githubDecoder)


all : Test
all =
    describe "GitHub Response Decoder"
        [ test "it results in an Err for invalid JSON" <|
            \() ->
                let
                    json =
                        """{ "pizza": [] }"""

                    isErrorResult result =
                        case result of
                            Ok _ ->
                                False

                            Err _ ->
                                True
                in
                    json
                        |> Json.Decode.decodeString githubDecoder
                        |> isErrorResult
                        |> Expect.true "Expected decoding an invalid response to return an Err."
        , test "it successfully decodes a valid response" <|
            \() ->
                """{ "items": [
                          { "id" : 5, "full_name" : "foo", "stargazers_count" : 42 }
                          , { "id" : 3, "full_name"  : "bar", "stargazers_count" :77 }

                 ] }"""
                    |> Json.Decode.decodeString githubDecoder
                    |> Expect.equal
                        (Ok
                            [ { id = 5, name = "foo", stars = 42 }
                            , { id = 3, name = "bar", stars = 77 }
                            ]
                        )
        , fuzz (list int) "it decodes one SearchResult for each 'item' in the JSON" <|
            \randomIds ->
                let
                    jsonFromId id =
                        """{"id": """ ++ toString id ++ """, "full_name": "foo", "stargazers_count": 42}"""

                    jsonItems =
                        String.join ", " (List.map jsonFromId randomIds)

                    json =
                        """{ "items": [""" ++ jsonItems ++ """] }"""
                in
                    case Json.Decode.decodeString githubDecoder json of
                        Ok results ->
                            List.length results
                                |> Expect.equal (List.length randomIds)

                        Err err ->
                            Expect.fail ("JSON decoding failed unexpectedly: " ++ err)
        ]
