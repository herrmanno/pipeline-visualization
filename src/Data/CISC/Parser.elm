module Data.CISC.Parser exposing (parseProgram)

{-| Parser for x86_64 programs
-}

import String exposing (lines, replace)
import Hex as Hex
import Data.Assembly exposing (Assembly, Instruction(..), Argument(..))
import Parser as P
import Parser exposing ((|.), (|=))
import Set
import Bitwise exposing (shiftLeftBy)

parseProgram : String -> Result (List P.DeadEnd) Assembly
parseProgram s =
    let xs = lines s
        f a b = case (a,b) of
            (_,(Err e)) -> Err e
            ((Err e),_) -> Err e
            ((Ok x),(Ok acc)) -> Ok(x :: acc)
    in
        List.map (P.run parseInstruction << replace "\t" " ") xs
        |> List.foldr f (Ok [])
        |> Result.map (List.filterMap identity)

parseInstruction : P.Parser (Maybe Instruction)
parseInstruction =
    let a = 0
    in
    P.oneOf
        [ P.backtrackable <| P.map Just <| P.succeed Instruction
            |. P.spaces
            |. P.chompWhile Char.isHexDigit
            |. P.symbol ":"
            |. P.chompUntil "  "
            |. P.spaces
            |= P.getChompedString (P.chompWhile Char.isAlphaNum)
            |. P.spaces
            |= parseArguments
            |. P.chompUntilEndOr "\n"
        , P.succeed Nothing
        ]

parseArgument : P.Parser Argument
parseArgument =
    let
        {- | Parses a hex num without '0x' prefix -}
        parseHex : P.Parser Int
        parseHex =
            P.succeed (Result.withDefault 0 << Hex.fromString) |= P.getChompedString (P.chompWhile Char.isHexDigit)

        {- in AT&T x86_64 all(!) numbers seem to be hex,
           sometimes with '0x' prefix (in case of immediates)
           and sometimes without (e.g. for unconditional jumps)
        -}
        parseNum =
            let
                oxHex = P.number { int = Nothing, hex = Just identity, octal = Nothing, binary = Nothing, float = Nothing }
                num = P.oneOf [P.backtrackable oxHex, parseHex]
            in P.oneOf [ P.succeed negate |. P.symbol "-" |= num , num ]
        parseRegister = P.succeed Register |= P.variable { start = ((==)'%'), inner = Char.isAlphaNum, reserved = Set.empty }
        parseImmediate = P.succeed Immediate |= P.oneOf [P.succeed identity |. P.symbol "$" |= parseNum, parseNum]
        parseAddress = P.succeed Address |= parseNum |. P.symbol "(" |= parseRegister |. P.symbol ")"
        parseTriple = P.succeed AddressTriple
            |. P.symbol "("
            |= parseRegister
            |. P.symbol ","
            |= parseRegister
            |. P.symbol ","
            |= parseImmediate
            |. P.symbol ")"
    in
        P.oneOf (List.map P.backtrackable [ parseTriple, parseAddress, parseRegister, parseImmediate])

parseArguments : P.Parser (List Argument)
parseArguments =
    let
        parseMore a =
            P.map ((::) a) <|
                P.oneOf
                    [ P.succeed identity |. P.symbol "," |= parseArguments
                    , P.succeed []
                    ]
    in
        P.oneOf
            [ P.andThen parseMore parseArgument
            , P.succeed []
            ]