module EventHandlers exposing (..)

import Json.Decode
import Html.Events
import Html exposing (Attribute)


-- CUSTOM EVENT HANDLERS


onBlurWithTargetValue : (String -> msg) -> Attribute msg
onBlurWithTargetValue toMsg =
    Html.Events.on "blur" (Json.Decode.map toMsg Html.Events.targetValue)


onChange : (String -> msg) -> Attribute msg
onChange toMsg =
    Html.Events.on "change" (Json.Decode.map toMsg Html.Events.targetValue)
