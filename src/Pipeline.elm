module Pipeline exposing (Pipeline, PipelineStep, PipelinePhase(..), buildPipeline, viewPipeline)

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict
import Data.Assembly exposing (Assembly, Instruction(..), Argument(..), registerName, instructionToString)
import Data.RISC exposing (..)

type alias Pipeline = List PipelineStep

type alias PipelineStep =
    { instruction : Instruction
    , phases : List PipelinePhase
    , offset : Int
    -- , blocked : List Argument
    , blocked : Dict.Dict String BlockingInformation
    }

type alias BlockingInformation = { index: Int, instr: Instruction, phase: Int }

blockingInfoToString : String -> BlockingInformation -> String
blockingInfoToString regName { index, instr, phase } =
    regName
    ++ " blocked by "
    ++ "(" ++ String.fromInt index ++ ") "
    ++ instructionToString instr
    ++ " until end of cycle "
    ++ String.fromInt phase

type PipelinePhase
    = Bubble
    | Fetch
    | Decode
    | Execute
    | Memory
    | Writeback

buildPipeline : Assembly -> Pipeline
buildPipeline instrs =
    let
        -- dictionare of 'registername' -> 'written after cycle n' usages
        dict : Dict.Dict String BlockingInformation
        dict = Dict.empty

        -- fold over instructions. Creates a single pipeline strap from instructions and context
        f instr (xs, (index, offset, usages)) =
            let
                paramUsages = getParameterUsage offset instr
                writeUsages =
                    List.filterMap
                        (\u ->
                            Maybe.map2
                                Tuple.pair
                                (registerName u.register)
                                (isWrite u.usage
                                    |> Maybe.map (\phase -> { index = index, instr = instr, phase = phase })))
                        paramUsages
                newUsages = List.foldr (\(k,v) d -> Dict.insert k v d) usages writeUsages
                numBubbles =
                    let
                        getWaitMaybe r i =
                            Dict.get r usages
                            |> Maybe.map (\b -> b.phase)
                            |> Maybe.andThen (\t -> if t - i > 0 then Just (t - i) else Nothing)
                    in
                    List.filterMap
                        (\u -> case (u.register, u.usage) of
                                    (Register r, Read i) -> getWaitMaybe r i
                                    (Register r, Write i) -> getWaitMaybe r i
                                    _ -> Nothing)
                        paramUsages
                        |> List.maximum
                        |> Maybe.withDefault 0
                blocked =
                    let
                        getBlockingMaybe reg i =
                            reg
                            |> Maybe.andThen (\r -> Dict.get r usages)
                            |> Maybe.andThen (\b -> if b.phase - i > 0 then Just b else Nothing)
                    in
                    List.filterMap
                        (\u ->
                            Maybe.map2 Tuple.pair
                            (registerName u.register)
                            (getBlockingMaybe (registerName u.register) (usageCycle u.usage)))
                        paramUsages
                    |> Dict.fromList
                newOffset = offset + 1 + numBubbles
                step =
                    { instruction = instr
                    , phases =
                        [Fetch, Decode]
                        ++ List.repeat numBubbles Bubble
                        ++ [Execute, Memory, Writeback]
                    , offset = offset
                    , blocked = blocked
                    }
            in (step :: xs, (index + 1, newOffset, newUsages))
    in List.foldl f ([], (0, 0, dict)) instrs
        |> Tuple.first
        |> List.reverse

viewPipeline : Int -> Pipeline -> Html a
viewPipeline wrap steps =
    let
        viewStep index { instruction, phases, offset, blocked } =
            let
                blockReason =
                    Dict.toList blocked
                    |> List.map (\(r, b) -> blockingInfoToString r b)
                    |> String.join "\n"
            in
            tr []
                (td [class "index"] [text (String.fromInt index)]
                :: viewInstruction instruction
                :: List.repeat (modBy wrap offset) (td [class "offset"] [])
                ++ (List.indexedMap (viewPhase offset blockReason) phases)
                )
        viewInstruction instr =
            td
                [class "command"]
                [code [] [text <| instructionToString instr]]
        viewArgument arg =
            case arg of
                Address offset register ->
                    span [] [text <| String.fromInt offset, text "(", viewArgument register, text ")"]
                Register name -> span [] [text name]
                Immediate value -> text <| String.fromInt value
        viewPhase offset blockReason index phase =
            let
                content s =
                    [ span [] [text s]
                    , span [class "phase__index"] [text <| String.fromInt (offset + index)]
                    ]
            in
            case phase of
                Fetch -> td [] (content "IF")
                Decode -> td [] (content "ID")
                Execute -> td [] (content "X")
                Memory -> td [] (content "M")
                Writeback -> td [] (content "WB")
                Bubble -> td [class "wait", title blockReason, style "cursor" "help"] (content "")
    in table [] (List.indexedMap viewStep steps)