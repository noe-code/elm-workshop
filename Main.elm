module Main exposing (..)

import Elmhub exposing (Model, Msg)
import Html exposing (program)


-- MAIN


main : Program Never Elmhub.Model Elmhub.Msg
main =
    Html.program
        { init = Elmhub.init
        , view = Elmhub.view
        , update = Elmhub.update
        , subscriptions = Elmhub.subscriptions
        }
