module RISCParser exposing (parseProgram)
import String exposing (lines, replace)
import Data.Assembly exposing (Assembly, Instruction(..), Argument(..))
import Parser as P
import Parser exposing ((|.), (|=))
import Set

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
    P.oneOf
        [ P.backtrackable <| P.map Just <| P.succeed Instruction
            |. P.spaces
            |. P.chompWhile Char.isHexDigit
            |. P.symbol ":"
            |. P.spaces
            |. P.chompWhile Char.isHexDigit
            |. P.spaces
            |= P.getChompedString (P.chompWhile Char.isAlphaNum)
            |. P.spaces
            |= parseArguments
            |. P.chompUntilEndOr "\n" -- TODO: parse arguments
        , P.succeed Nothing
        ]

parseArgument : P.Parser Argument
parseArgument =
    let
        parseInt = P.oneOf [ P.succeed negate |. P.symbol "-" |= P.int , P.int ]
        parseRegister = P.succeed Register |= P.variable { start = Char.isAlpha, inner = Char.isAlphaNum, reserved = Set.empty }
        parseImmediate = P.succeed Immediate |= parseInt
        parseAddress = P.succeed Address |= parseInt |. P.symbol "(" |= parseRegister |. P.symbol ")"
    in
        P.oneOf (List.map P.backtrackable [ parseAddress, parseRegister, parseImmediate])

-- P.getChompedString (P.chompWhile (\c -> c /= ',' && c /= ' '))

parseArguments : P.Parser (List Argument)
parseArguments =
    let
        -- parseMore : String -> P.Parser (List String)
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