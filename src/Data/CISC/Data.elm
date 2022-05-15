module Data.CISC.Data exposing (getParameterUsage, exampleCode)

{-| CISC specific calculation of bubbles
-}

import Data.Assembly exposing (..)

-- Helper functions
read : Argument -> Int -> ParameterUsage
read r o = { register = r, usage = Read o}

write : Argument -> Int -> ParameterUsage
write r o = { register = r, usage = Write o}

{-| Calculate read/write usages for instructions

Because x86_64 has a ton(!) of instructions it is to tedious to list them all.
Therefore the usage of parameters is only *guessed*, based on the number and kind of parameters,
not regarding the speicific op-code at all.
-}
getParameterUsage : Int -> Instruction -> ParameterUsages
getParameterUsage offset (Instruction _ args) =
    let
        afterExecute = offset + 2
        afterMemory = offset + 3
        onDecode = offset + 1
    in
        case args of
            [Register r] ->
                [read (Register r) afterMemory]
            [Address _ r] ->
                [read r afterMemory]
            [Register r1, Register r2] ->
                [write (Register r1) afterExecute, read (Register r2) onDecode]
            [Register r1, Address _ r2] ->
                [write (Register r1) afterMemory, read r2 onDecode]
            [Address _ r1, Register r2] ->
                [write (Register r2) afterMemory, read r1 onDecode]
            [Address _ r1, Address _ r2] ->
                [write r1 afterMemory, read r2 onDecode]
            [AddressTriple r1 r2 _, Address _ r3] ->
                [write r3 afterExecute, read r1 onDecode, read r2 onDecode]
            _ -> []

exampleCode : String
exampleCode = """
build/debug/CMakeFiles/libfib3.dir/fib3.c.o:     file format elf64-x86-64


Disassembly of section .text:

0000000000000000 <fib>:

#define LOOP (b = a + b, a = b - a)
#define LOOP2 (LOOP, LOOP)
#define LOOP4 (LOOP2, LOOP2)

uint64_t fib(uint32_t n) {
   0:	55                   	push   %rbp
   1:	48 89 e5             	mov    %rsp,%rbp
   4:	41 54                	push   %r12
   6:	53                   	push   %rbx
   7:	89 7d ec             	mov    %edi,-0x14(%rbp)
    register uint64_t t = 0;
    register uint64_t a = 0;
   a:	41 bc 00 00 00 00    	mov    $0x0,%r12d
    register uint64_t b = 1;
  10:	bb 01 00 00 00       	mov    $0x1,%ebx

    start:
    if (n <= 1) {
  15:	83 7d ec 01          	cmpl   $0x1,-0x14(%rbp)
  19:	77 05                	ja     20 <fib+0x20>
        return a;
  1b:	4c 89 e0             	mov    %r12,%rax
  1e:	eb 4e                	jmp    6e <fib+0x6e>
    } else if (n > 4) {
  20:	83 7d ec 04          	cmpl   $0x4,-0x14(%rbp)
  24:	76 36                	jbe    5c <fib+0x5c>
        LOOP4;
  26:	4c 01 e3             	add    %r12,%rbx
  29:	48 89 d8             	mov    %rbx,%rax
  2c:	4c 29 e0             	sub    %r12,%rax
  2f:	49 89 c4             	mov    %rax,%r12
  32:	4c 01 e3             	add    %r12,%rbx
  35:	48 89 d8             	mov    %rbx,%rax
  38:	4c 29 e0             	sub    %r12,%rax
  3b:	49 89 c4             	mov    %rax,%r12
  3e:	4c 01 e3             	add    %r12,%rbx
  41:	48 89 d8             	mov    %rbx,%rax
  44:	4c 29 e0             	sub    %r12,%rax
  47:	49 89 c4             	mov    %rax,%r12
  4a:	4c 01 e3             	add    %r12,%rbx
  4d:	48 89 d8             	mov    %rbx,%rax
  50:	4c 29 e0             	sub    %r12,%rax
  53:	49 89 c4             	mov    %rax,%r12
        n -= 4;
  56:	83 6d ec 04          	subl   $0x4,-0x14(%rbp)
        goto start;
  5a:	eb b9                	jmp    15 <fib+0x15>
    } else {
        LOOP;
  5c:	4c 01 e3             	add    %r12,%rbx
  5f:	48 89 d8             	mov    %rbx,%rax
  62:	4c 29 e0             	sub    %r12,%rax
  65:	49 89 c4             	mov    %rax,%r12
        n -= 1;
  68:	83 6d ec 01          	subl   $0x1,-0x14(%rbp)
        goto start;
  6c:	eb a7                	jmp    15 <fib+0x15>
    }
}
  6e:	5b                   	pop    %rbx
  6f:	41 5c                	pop    %r12
  71:	5d                   	pop    %rbp
  72:	c3                   	retq   
"""