module Data.RISC exposing (..)

import Data.Assembly exposing (..)

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

read : Argument -> Int -> ParameterUsage
read r o = { register = r, usage = Read o}

write : Argument -> Int -> ParameterUsage
write r o = { register = r, usage = Write o}

usageCycle : Usage -> Int
usageCycle u =
    case u of
        Write i -> i
        Read i -> i

type alias AtStartOfCycle = Int
type alias AtEndOfCycle = Int

-- | see http://csl.snu.ac.kr/courses/4190.307/2020-1/riscv-user-isa.pdf
getParameterUsage : Int -> Instruction -> ParameterUsages
getParameterUsage offset (Instruction itype args) =
    let
        afterExecute = offset + 2
        afterMemory = offset + 3
        onDecode = offset + 1
    in
    case (itype, args) of
        ("add", [rd, rs1, rs2]) ->
            [write rd afterExecute, read rs1 onDecode, read rs2 onDecode]
        ("sub", [rd, rs1, rs2]) ->
            [write rd afterExecute, read rs1 onDecode, read rs2 onDecode]
        ("addi", [rd, rs1, _]) ->
            [write rd afterExecute, read rs1 onDecode]
        ("slt", [rd, rs1, rs2]) ->
            [write rd afterExecute, read rs1 onDecode, read rs2 onDecode]
        ("slti", [rd, rs1, _]) ->
            [write rd afterExecute, read rs1 onDecode]
        ("sltu", [rd, rs1, rs2]) ->
            [write rd afterExecute, read rs1 onDecode, read rs2 onDecode]
        ("lui", [rd, _]) ->
            [write rd afterExecute]
        ("lui", [rd]) ->
            [write rd afterExecute, read (Register "pc") onDecode]
        ("and", [rd, rs1, rs2]) ->
            [write rd afterExecute, read rs1 onDecode, read rs2 onDecode]
        ("or", [rd, rs1, rs2]) ->
            [write rd afterExecute, read rs1 onDecode, read rs2 onDecode]
        ("xor", [rd, rs1, rs2]) ->
            [write rd afterExecute, read rs1 onDecode, read rs2 onDecode]
        ("andi", [rd, rs1]) ->
            [write rd afterExecute, read rs1 onDecode]
        ("ori", [rd, rs1]) ->
            [write rd afterExecute, read rs1 onDecode]
        ("xori", [rd, rs1]) ->
            [write rd afterExecute, read rs1 onDecode]
        ("sll", [rd, rs1, rs2]) ->
            [write rd afterExecute, read rs1 onDecode, read rs2 onDecode]
        ("srl", [rd, rs1, rs2]) ->
            [write rd afterExecute, read rs1 onDecode, read rs2 onDecode]
        ("sra", [rd, rs1, rs2]) ->
            [write rd afterExecute, read rs1 onDecode, read rs2 onDecode]
        ("slli", [rd, rs1]) ->
            [write rd afterExecute, read rs1 onDecode]
        ("srli", [rd, rs1]) ->
            [write rd afterExecute, read rs1 onDecode]
        ("srai", [rd, rs1]) ->
            [write rd afterExecute, read rs1 onDecode]
        ("ld", [rd, Address _ rs1]) ->
            [write rd afterMemory, read rs1 onDecode]
        ("lw", [rd, Address _ rs1]) ->
            [write rd afterMemory, read rs1 onDecode]
        ("lh", [rd, Address _ rs1]) ->
            [write rd afterMemory, read rs1 onDecode]
        ("lb", [rd, Address _ rs1]) ->
            [write rd afterMemory, read rs1 onDecode]
        ("lwu", [rd, Address _ rs1]) ->
            [write rd afterMemory, read rs1 onDecode]
        ("lhu", [rd, Address _ rs1]) ->
            [write rd afterMemory, read rs1 onDecode]
        ("lbu", [rd, Address _ rs1]) ->
            [write rd afterMemory, read rs1 onDecode]
        ("sd", [rs1, Address _ rs2]) ->
            [read rs2 onDecode, read rs1 onDecode]
        ("sw", [rs1, Address _ rs2]) ->
            [read rs2 onDecode, read rs1 onDecode]
        ("sh", [rs1, Address _ rs2]) ->
            [read rs2 onDecode, read rs1 onDecode]
        ("sb", [rs1, Address _ rs2]) ->
            [read rs2 onDecode, read rs1 onDecode]
        ("beq", [rs1, rs2, _]) ->
            [read rs1 onDecode, read rs2 onDecode]
        ("bne", [rs1, rs2, _]) ->
            [read rs1 onDecode, read rs2 onDecode]
        ("bge", [rs1, rs2, _]) ->
            [read rs1 onDecode, read rs2 onDecode]
        ("bgeu", [rs1, rs2, _]) ->
            [read rs1 onDecode, read rs2 onDecode]
        ("blt", [rs1, rs2, _]) ->
            [read rs1 onDecode, read rs2 onDecode]
        ("bltu", [rs1, rs2, _]) ->
            [read rs1 onDecode, read rs2 onDecode]
        ("jal", [rd, _]) ->
            [write rd afterExecute]
        ("jalr", [rd, Address _ rs1]) ->
            [read rs1 onDecode, write rd afterExecute]
        ("j", [_]) ->
            []
        ("call", [_]) ->
            []
        ("ret", []) ->
            []
        ("sext", []) ->
            []
        ("mv", [rd, rs1]) -> getParameterUsage offset (Instruction "addi" [rd, rs1, Immediate 0])
        ("li", [rd, imm]) -> getParameterUsage offset (Instruction "addi" [rd, Register "x0", imm])
        ("addiw", [rd, rs1, _]) ->
            [write rd afterExecute, read rs1 onDecode]
        ("bgt", [rs, rt, os]) -> getParameterUsage offset (Instruction "bgl" [rt, rs, os])
        ("bgtu", [rs, rt, os]) -> getParameterUsage offset (Instruction "bglu" [rt, rs, os])
        ("ble", [rs, rt, os]) -> getParameterUsage offset (Instruction "bge" [rt, rs, os])
        ("bleu", [rs, rt, os]) -> getParameterUsage offset (Instruction "bgeu" [rt, rs, os])

        ("beqz", [rs, os]) -> getParameterUsage offset (Instruction "beq" [rs, os])
        ("bnez", [rs, os]) -> getParameterUsage offset (Instruction "bne" [rs, os])
        ("bgez", [rs, os]) -> getParameterUsage offset (Instruction "bge" [rs, os])
        ("bltz", [rs, os]) -> getParameterUsage offset (Instruction "blt" [rs, os])
        ("blez", [rs, os]) -> getParameterUsage offset (Instruction "ble" [rs, os])
        ("bgtz", [rs, os]) -> getParameterUsage offset (Instruction "bgt" [rs, os])


        _ -> Debug.log ("Unknown instruction: " ++ Debug.toString (Instruction itype args)) []
