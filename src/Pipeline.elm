module Pipeline exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Dict
import Data.Assembly exposing (Assembly, Instruction(..))
import Data.Assembly exposing (Argument(..))
import Data.RISC exposing (..)

type alias Pipeline = List PipelineStep

type alias PipelineStep = { instruction : Instruction, phases : List PipelinePhase, offset : Int, blocked : List Argument }

type PipelinePhase
    = Bubble
    | Fetch
    | Decode
    | Execute
    | Memory
    | Writeback

type alias PipelineBuildOptions = { stepWrap : Int }

buildPipeline : PipelineBuildOptions -> Assembly -> Pipeline
buildPipeline { stepWrap } instrs =
    let dict : Dict.Dict String Int
        dict = Dict.empty -- dictionare of 'registername' -> 'written after cycle n' usages
        f instr (xs, (offset, visualOffset, usages)) =
         let paramUsages = getParameterUsage offset instr
             writeUsages =
                    List.filterMap
                        (\u -> case (u.register, u.usage) of
                                    (Register r, Write i) -> Just (r, i)
                                    _ -> Nothing)
                        paramUsages
             readUsages =
                    List.filterMap
                        (\u -> case (u.register, u.usage) of
                                    (Register r, Read i) -> Just (r, i)
                                    _ -> Nothing)
                        paramUsages
             newUsages = List.foldr (\(k,v) d -> Dict.insert k v d) usages writeUsages
             -- TODO: add reason for waiting
             numBubbles =
                List.filterMap
                    (\u -> case (u.register, u.usage) of
                                (Register r, Read i) -> Dict.get r usages |> Maybe.map (\t -> t - i) |> Maybe.andThen (\t -> if t > 0 then Just t else Nothing)
                                (Register r, Write i) -> Dict.get r usages |> Maybe.map (\t -> t - i) |> Maybe.andThen (\t -> if t > 0 then Just t else Nothing)
                                _ -> Nothing)
                    paramUsages
                    |> List.maximum
                    |> Maybe.withDefault 0
             blockedRegisterNames =
                List.filterMap
                    (\u -> case (u.register, u.usage) of
                                (Register r, Read i) -> Dict.get r usages |> Maybe.map (\t -> t - i) |> Maybe.andThen (\t -> if t > 0 then Just r else Nothing)
                                (Register r, Write i) -> Dict.get r usages |> Maybe.map (\t -> t - i) |> Maybe.andThen (\t -> if t > 0 then Just r else Nothing)
                                _ -> Nothing)
                    paramUsages
             newOffset = offset + 1 + numBubbles
             newVisualOffset = if visualOffset > stepWrap then 0 else visualOffset + 1 + numBubbles
             step =
                { instruction = instr
                , phases = [Fetch, Decode] ++ List.repeat numBubbles Bubble ++ [Execute, Memory, Writeback]
                , offset = visualOffset
                , blocked = List.map Register blockedRegisterNames
                }
         in (step :: xs, (newOffset, newVisualOffset, newUsages))
    in List.foldl f ([], (0, 0, dict)) instrs
        |> Tuple.first
        |> List.reverse

viewPipeline : Pipeline -> Html a
viewPipeline steps =
    let
        viewStep index { instruction, phases, offset, blocked } =
            tr []
                (td [class "index"] [text (String.fromInt index)]
                :: viewInstruction blocked instruction
                :: List.repeat offset (td [class "offset"] [])
                ++ (List.map viewPhase phases)
                )
        viewInstruction blocked (Instruction cmd args) =
            td [class "command"] (text cmd :: text " " :: List.intersperse (text ", ") (List.map (viewArgument blocked) args))
        viewArgument blocked arg =
            case arg of
                Address offset register ->
                    span [] [text <| String.fromInt offset, text "(", viewArgument blocked register, text ")"]
                Register name -> span [classList [("dependency", List.member (Register name) blocked)]] [text name]
                Immediate value -> text <| String.fromInt value
        viewPhase phase =
            case phase of
                Fetch -> td [] [text "IF"]
                Decode -> td [] [text "ID"]
                Execute -> td [] [text "X"]
                Memory -> td [] [text "M"]
                Writeback -> td [] [text "WB"]
                Bubble -> td [class "wait"] []
    in
        table
            []
            (List.indexedMap viewStep steps)