module Data.Assembly exposing
    ( Architecture(..)
    , fromString
    , toString
    , Assembly
    , InstructionType
    , Instruction(..)
    , instructionToString
    , Argument(..)
    , argumentToString
    , registerName
    , ParameterUsages
    , ParameterUsage
    , Usage(..)
    , isRead
    , isWrite
    , usageCycle
    )
import Hex

type Architecture = RISC | CISC

toString : Architecture -> String
toString a =
    case a of
        RISC -> "RISC"
        CISC -> "CISC"

fromString : String -> Maybe Architecture
fromString s =
    case s of
        "RISC" -> Just RISC
        "CISC" -> Just CISC
        _ -> Nothing



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
    | AddressTriple Argument Argument Argument

argumentToString : Argument -> String
argumentToString a =
    case a of
        Register name -> name
        Immediate value -> (if value < 0 then "-" else "") ++ "0x" ++ Hex.toString (abs value)
        Address os r -> argumentToString (Immediate os) ++ "(" ++ argumentToString r ++ ")"
        AddressTriple x y z ->
            "(" ++ argumentToString x ++ "," ++ argumentToString y ++ "," ++ argumentToString z ++ ")"

registerName : Argument -> Maybe String
registerName a =
    case a of
        Register s -> Just s
        _ -> Nothing

type alias ParameterUsages = List ParameterUsage
type alias ParameterUsage = { register : Argument, usage : Usage }
type Usage = Read AtStartOfCycle | Write AtEndOfCycle

isRead : Usage -> Maybe AtStartOfCycle
isRead usage = case usage of
    Read i -> Just i
    _ -> Nothing

isWrite : Usage -> Maybe AtEndOfCycle
isWrite usage = case usage of
    Write i -> Just i
    _ -> Nothing

usageCycle : Usage -> Int
usageCycle u =
    case u of
        Write i -> i
        Read i -> i

type alias AtStartOfCycle = Int
type alias AtEndOfCycle = Int