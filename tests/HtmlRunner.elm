module HtmlRunner exposing (..)

import Tests
import Test.Runner.Html exposing (run, TestProgram)


--main : Program Never Model Msg


main : TestProgram
main =
    run Tests.all
