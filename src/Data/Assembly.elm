module Data.Assembly exposing (..)


type alias Assembly = List Instruction

type alias InstructionType = String

type Instruction = Instruction InstructionType (List Argument)

instructionToString : Instruction -> String
instructionToString (Instruction itype args) =
    itype ++ " " ++ String.join "," (List.map argumentToString args)

type Argument
    = Register String -- ^ name
    | Immediate Int --^ value
    | Address Int Argument --^ offset, register

argumentToString : Argument -> String
argumentToString a =
    case a of
        Register name -> name
        Immediate value -> String.fromInt value
        Address os r -> String.fromInt os ++ "(" ++ argumentToString r ++ ")"

registerName : Argument -> Maybe String
registerName a =
    case a of
        Register s -> Just s
        _ -> Nothing