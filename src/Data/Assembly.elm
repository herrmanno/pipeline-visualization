module Data.Assembly exposing (..)


type alias Assembly = List Instruction

type alias InstructionType = String

type Instruction = Instruction InstructionType (List Argument)

type Argument
    = Register String -- ^ name
    | Immediate Int --^ value
    | Address Int Argument --^ offset, register