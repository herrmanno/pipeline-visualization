port module Main exposing (main)

import Browser exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Attributes as A
import Html.Events exposing (..)
import Parser as P

import Data.Assembly exposing (Architecture(..), toString, fromString)
import Data.RISC.Data as RISC
import Data.CISC.Data as CISC
import Data.RISC.Parser as RISCParser
import Data.CISC.Parser as CISCParser
import Pipeline exposing (Pipeline, buildPipeline, viewPipeline)
import Data.CISC.Data exposing (getParameterUsage)

type alias Flags = { arch: Maybe String, code : Maybe String }

main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { architecture : Architecture
    , code : String
    , pipeline : Result (List P.DeadEnd) Pipeline
    , stepWrap : Int
    , inputAreaVisible : Bool
    }

type Msg
    = UpdateArchitecture Architecture
    | UpdateCode String
    | UpdateStepWrap Int
    | ToggleInputArea

port setStorage : (String, String) -> Cmd msg

init : Flags -> (Model, Cmd Msg)
init flags =
    let defaultModel =
            { architecture = CISC
            , code = ""
            , pipeline = Err []
            , stepWrap = 20
            , inputAreaVisible = True
            }
        arch = Maybe.withDefault CISC (Maybe.andThen fromString flags.arch)
        code = Maybe.withDefault "" flags.code
        parseProgram =
            if arch == RISC then RISCParser.parseProgram else CISCParser.parseProgram
        getParameterUsage =
            if defaultModel.architecture == RISC then RISC.getParameterUsage else CISC.getParameterUsage
    in
        ( { defaultModel
          | architecture = arch
          , code = code
          , pipeline = (Result.map (buildPipeline getParameterUsage)) << parseProgram <| code }
        , Cmd.none)

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    let parseProgram =
            if model.architecture == RISC then RISCParser.parseProgram else CISCParser.parseProgram
    in
    case msg of
        UpdateArchitecture arch ->
            ({ model | architecture = arch, code = "", pipeline = Err [] }, setStorage ("arch", toString arch))
        UpdateCode code ->
            let
                getParameterUsage =
                    if model.architecture == RISC then RISC.getParameterUsage else CISC.getParameterUsage
                pipeline = parseProgram code |> Result.map (buildPipeline getParameterUsage)
            in ({model | code = code, pipeline = pipeline }, setStorage ("code", code))
        UpdateStepWrap stepWrap ->
            ({model | stepWrap = stepWrap }, Cmd.none)
        ToggleInputArea ->
            ({ model | inputAreaVisible = not model.inputAreaVisible }, Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none

view : Model -> Html Msg
view model =
    main_ []
        [ viewHeader
        , viewInputArea model
        , viewProgram model
        ]

viewHeader : Html Msg
viewHeader =
    div []
        [ h1 [class "header__title"] [text "Assembly Pipeline Visualizer"]
        ]

viewInputArea : Model -> Html Msg
viewInputArea model =
    let
        exampleCode = if model.architecture == RISC then RISC.exampleCode else CISC.exampleCode
    in
    div []
        [ hr [] []
        , div
            [class "inputarea"
            , classList [("inputarea--hidden", not model.inputAreaVisible)]
            ]
            [ div []
                [ img
                    [ class "inputarea__toggle"
                    , src "./images/double_chevron_up.png"
                    , title "Collapse / Show"
                    , onClick ToggleInputArea] [] 
                ]
            , div [class "inputarea__left"]
                [ label []
                    [ text "Architecture "
                    , select
                        [ onInput (UpdateArchitecture << Maybe.withDefault CISC << fromString)]
                        [ option [value (toString CISC), selected (model.architecture == CISC)]
                            [ text (toString CISC ++ " (AT&T syntax)")]
                        , option [value (toString RISC), selected (model.architecture == RISC)]
                            [ text (toString RISC)]
                        ]

                    ]
                , textarea
                    [ placeholder "Insert output from 'objdump' here"
                    , cols 120
                    , rows 30
                    , value model.code
                    , onInput UpdateCode
                    ] []
                , button [onClick (UpdateCode exampleCode)] [text "Load example program"]
                ]
            , div [class "inputarea__right"]
                [ h3 [] [text "How to use"]
                , p [] [text
                    """Copy your output from 'objdump' into the textarea on the left and, if desired, edit it.
                       On Every change the current pipeline will be generated and shown below.
                    """]
                , h4 [] [text "How to create this 'objdump output'"]
                , p [] [text
                    """
                    Assume you have a file `file.c`, which contains your program. Then run
                    `gcc -c file.c -o file.o` to create an object file from it (you may add more
                    flags to gcc as '-g' or '-O3').
                    Afterwards invoke `objdump -d --source file.o` and save its output to another
                    file or paste it here directly.
                    """]
                , h4 [] [text "How to compile to RISCV code"]
                , p [] [text
                    """
                    The RISCV toolchain (containing a version of gcc and objdump able to create
                    the desired output) can be found online. Also, it is available at the 
                    """
                    , span [style "textDecoration" "underline"] [text "HTWK linux machine (simson pool)"]
                    , text
                    """
                     ,probably at `/home/jmuell12/Downloads/riscv64-unknown-elf-gcc-8.3.0-2020.04.1-x86_64-linux-ubuntu14/bin/`.
                    """]
                , h4 [] [text "How the pipeline is generated"]
                , p [] [text
                    """
                    The pipeline contains one fetch, decode, execute, memory and writeback phase for
                    every instruction. Also it adds bubble phases, when it encounters a instruction,
                    that needs to wait before reading from or writing to a register.
                    This information is calculated based on the former instructions.
                    Every instruction defines at which phase it may read or write a register.
                    All registers it writes to are then locked till after that phase.
                    If a latter instruction wants to read or write to a currently locked register
                    it waits until the register is freed.
                    """]
                ]
            ]
        ]

viewProgram : Model -> Html Msg
viewProgram model =
    let
        viewDeadEnd {row, col, problem}
            = text <| "(" ++ String.fromInt row ++ "," ++ String.fromInt col ++ ") " ++ Debug.toString problem
    in
    case model.pipeline of
        Err e ->
            code [] (List.map viewDeadEnd e)
        Ok instrs ->
            div []
                [ hr [] []
                , br [] []
                , label [class "pipeline__step-control"]
                    [ text <| "Wrap table after " ++ String.fromInt model.stepWrap ++ " steps"
                    , input
                        [type_ "range", A.min "5", A.max "40", step "1"
                        , value (String.fromInt model.stepWrap)
                        , onInput (UpdateStepWrap << Maybe.withDefault model.stepWrap << String.toInt)]
                        []
                    ]
                , viewPipeline model.stepWrap instrs
                ]
