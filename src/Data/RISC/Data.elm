module Data.RISC.Data exposing (getParameterUsage, exampleCode)

{-| RISC specific calculation of bubbles
-}

import Data.Assembly exposing (..)

-- Helper functions
read : Argument -> Int -> ParameterUsage
read r o = { register = r, usage = Read o}

write : Argument -> Int -> ParameterUsage
write r o = { register = r, usage = Write o}

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


exampleCode = """
build/debug/fib3.risc.o:     file format elf64-littleriscv


Disassembly of section .text:

0000000000000000 <fib>:

#define LOOP (b = a + b, a = b - a)
#define LOOP2 (LOOP, LOOP)
#define LOOP4 (LOOP2, LOOP2)

uint64_t fib(uint32_t n) {
   0:	7179                	addi	sp,sp,-48
   2:	f422                	sd	s0,40(sp)
   4:	f026                	sd	s1,32(sp)
   6:	ec4a                	sd	s2,24(sp)
   8:	1800                	addi	s0,sp,48
   a:	87aa                	mv	a5,a0
   c:	fcf42e23          	sw	a5,-36(s0)
    register uint64_t t = 0;
    register uint64_t a = 0;
  10:	4901                	li	s2,0
    register uint64_t b = 1;
  12:	4485                	li	s1,1

0000000000000014 <.L2>:

    start:
    if (n <= 1) {
  14:	fdc42783          	lw	a5,-36(s0)
  18:	0007871b          	sext.w	a4,a5
  1c:	4785                	li	a5,1
  1e:	00e7e463          	bltu	a5,a4,26 <.L3>
        return a;
  22:	87ca                	mv	a5,s2
  24:	a099                	j	6a <.L6>

0000000000000026 <.L3>:
    } else if (n > 4) {
  26:	fdc42783          	lw	a5,-36(s0)
  2a:	0007871b          	sext.w	a4,a5
  2e:	4791                	li	a5,4
  30:	02e7f463          	bgeu	a5,a4,58 <.L5>
        LOOP4;
  34:	94ca                	add	s1,s1,s2
  36:	41248933          	sub	s2,s1,s2
  3a:	94ca                	add	s1,s1,s2
  3c:	41248933          	sub	s2,s1,s2
  40:	94ca                	add	s1,s1,s2
  42:	41248933          	sub	s2,s1,s2
  46:	94ca                	add	s1,s1,s2
  48:	41248933          	sub	s2,s1,s2
        n -= 4;
  4c:	fdc42783          	lw	a5,-36(s0)
  50:	37f1                	addiw	a5,a5,-4
  52:	fcf42e23          	sw	a5,-36(s0)
        goto start;
  56:	bf7d                	j	14 <.L2>

0000000000000058 <.L5>:
    } else {
        LOOP;
  58:	94ca                	add	s1,s1,s2
  5a:	41248933          	sub	s2,s1,s2
        n -= 1;
  5e:	fdc42783          	lw	a5,-36(s0)
  62:	37fd                	addiw	a5,a5,-1
  64:	fcf42e23          	sw	a5,-36(s0)
        goto start;
  68:	b775                	j	14 <.L2>

000000000000006a <.L6>:
    }
}
  6a:	853e                	mv	a0,a5
  6c:	7422                	ld	s0,40(sp)
  6e:	7482                	ld	s1,32(sp)
  70:	6962                	ld	s2,24(sp)
  72:	6145                	addi	sp,sp,48
  74:	8082                	ret
"""