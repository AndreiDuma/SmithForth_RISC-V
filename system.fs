: DBG   dbg BYE ;
: REG   reg BYE ;

\ Load current stack item into register `t0` or `t1`.
: >t0 ( x -- x )
  83 . B2 . 09 . 00 . ;					    \ t0 = [s3]               ld t0, 0(s3)
: >t1 ( x -- x )
  03 . B3 . 09 . 00 . ;					    \ t1 = [s3]               ld t1, 0(s3)

\ Store register `t0` or `t1` over current stack item.
: t0> ( ? -- x )
  23 . B0 . 59 . 00 . ;					    \ [s3] = t0               sd t0, 0(s3)
: t1> ( ? -- x )
  23 . B0 . 69 . 00 . ;					    \ [s3] = t1               sd t1, 0(s3)

\ Navigate the stack.
: ^ ( x -- .. )
  93 . 89 . 89 . 00 . ;                                     \ s3 += 8                 addi s3, s3, 8
: v ( .. -- x )
  93 . 89 . 89 . FF . ;                                     \ s3 -= 8                 addi s3, s3, -8

\ "Float" current stack item "up" the stack, exchanging with item
\ above.  Stack pointer follows the item.
: % ( x1 x2 -- x2 .. )
  >t1 ^  >t0 v
  t0> ^  t1> ;

: DROP ( x -- )                   [ ^ ] ;
: DUP  ( x -- x x )               [ >t0 v t0> ] ;
: SWAP ( x1 x2 -- x2 x1 )         [ % v ] ;
: OVER ( x1 x2 -- x1 x2 x1 )      [ ^ >t0 v v t0> ] ;
: ROT  ( x1 x2 x3 -- x2 x3 x1 )   [ ^ % v v % v ]  ;
: 2SWAP ( d1 d2 -- d2 d1 )        [ ^ % % v v v % % v v ] ;
: 2DUP ( d -- d d )               [ ^ >t0 v >t1 v t0> v t1> ] ;
: 2OVER ( d1 d2 -- d1 d2 d1 )     [ ^ ^ ^ >t0 v >t1 v v v t0> v t1> ] ;
: 2DROP ( d1 d2 -- d1 )           [ ^ ^ ] ;


\ --- RISC-V Assembler --- \

: << ( n u -- n' )
  [ >t1 ^ >t0 ]
  [ B3 . 92 . 62 . 00 . ]		\ sll t0, t0, t1
  [       t0> ] ;

: | ( u1 u2 -- u )
  [ >t1 ^ >t0 ]
  [ B3 . E2 . 62 . 00 . ]		\ or t0, t0, t1
  [       t0> ] ;

: & ( u1 u2 -- u )
  [ >t1 ^ >t0 ]
  [ B3 . F2 . 62 . 00 . ]		\ and t0, t0, t1
  [       t0> ] ;

: - ( n1 n2 -- n )
  [ >t1 ^ >t0 ]
  [ B3 . 82 . 62 . 40 . ]		\ sub t0, t0, t1
  [       t0> ] ;

\ Pick bits `i` (high) through `j` (low) from `n`.
: [:] ( u i j -- u' )
  [ >t1 ]		      \ t1 = j
  [ ^ ^ >t0 ]		      \ t0 = u                   ( -- u .. )
  [ B3 . D2 . 62 . 00 . ]     \ srl t0, t0, t1
  [ t0> v v ]		      \ v = t0                   ( -- v i j )
  1 -  -		      \ len = i - (j - 1)        ( -- v len )
  1 SWAP <<  1 -	      \ mask = (1 << len) - 1    ( -- v mask )
  & ;			      \ u' = v & mask            ( -- u' )

\ Compile a 32-bit instruction (in the form of a 32-bit unsigned
\ integer) to `OUTPUT`, ensuring correct endianness.
: ` ( u -- )
  DUP 1F 18 [:] SWAP  ( -- u[31:24] u )
  DUP 17 10 [:] SWAP  ( -- u[31:24] u[23:16] u )
  DUP 0F 08 [:] SWAP  ( -- u[31:24] u[23:16] u[15:8] u )
      07 00 [:]       ( -- u[31:24] u[23:16] u[15:8] u[7:0] )
  . . . . ;

\ Common format for R/S/B-type instructions.
: `instr/rsb ( op rd/imm5 fn3 rs1 rs2 fn7/imm7 -- )
  5 << | 5 << | 3 << | 5 << | 7 << | ` ;

\ R-type instructions.
: `instr/r ( rd rs1 rs2 fn7 fn3 op -- )
  [ % % % % % v v v v v ]  ( -- op rd rs1 rs2 fn7 fn3 )
  [ % % %         v v v ]  ( -- op rd fn3 rs1 rs2 fn7 )
  `instr/rsb ;
: `add  ( rd rs1 rs2 -- )   00 0 33 `instr/r ;
: `sub  ( rd rs1 rs2 -- )   20 0 33 `instr/r ;
: `sll  ( rd rs1 rs2 -- )   00 1 33 `instr/r ;
: `slt  ( rd rs1 rs2 -- )   00 2 33 `instr/r ;
: `sltu ( rd rs1 rs2 -- )   00 3 33 `instr/r ;
: `xor  ( rd rs1 rs2 -- )   00 4 33 `instr/r ;
: `srl  ( rd rs1 rs2 -- )   00 5 33 `instr/r ;
: `sra  ( rd rs1 rs2 -- )   20 5 33 `instr/r ;
: `or   ( rd rs1 rs2 -- )   00 6 33 `instr/r ;
: `and  ( rd rs1 rs2 -- )   00 7 33 `instr/r ;
\ RV64 instructions.
: `addw ( rd rs1 rs2 -- )   00 0 3B `instr/r ;
: `subw ( rd rs1 rs2 -- )   20 0 3B `instr/r ;
: `sllw ( rd rs1 rs2 -- )   00 1 3B `instr/r ;
: `srlw ( rd rs1 rs2 -- )   00 5 3B `instr/r ;
: `sraw ( rd rs1 rs2 -- )   20 5 3B `instr/r ;

\ I-type instructions.
: `instr/i ( rd rs1 imm fn3 op -- )
  [ % % % % v v v v ]  ( -- op rd rs1 imm fn3 )
  [ % %         v v ]  ( -- op rd fn3 rs1 imm )
  5 << | 3 << | 5 << | 7 << | ` ;
: `instr/i/shift ( rd rs1 shamt fn7 fn3 op -- )
  2SWAP		 ( -- rd rs1 fn3 op shamt fn7 )
  5 << |	 ( -- rd rs1 fn3 op imm )
  ROT ROT	 ( -- rd rs1 imm fn3 op )
  `instr/i ;
: `ecall ( -- )                0 0 000 0 73 `instr/i ;
: `jalr  ( rd rs1 imm   -- )           0 67 `instr/i ;
: `lb    ( rd rs1 imm   -- )           0 03 `instr/i ;
: `lh    ( rd rs1 imm   -- )           1 03 `instr/i ;
: `lw    ( rd rs1 imm   -- )           2 03 `instr/i ;
: `lbu   ( rd rs1 imm   -- )           4 03 `instr/i ;
: `lhu   ( rd rs1 imm   -- )           5 03 `instr/i ;
: `addi  ( rd rs1 imm   -- )           0 13 `instr/i ;
: `slti  ( rd rs1 imm   -- )           2 13 `instr/i ;
: `sltiu ( rd rs1 imm   -- )           3 13 `instr/i ;
: `xori  ( rd rs1 imm   -- )           4 13 `instr/i ;
: `ori   ( rd rs1 imm   -- )           6 13 `instr/i ;
: `andi  ( rd rs1 imm   -- )           7 13 `instr/i ;
: `slli  ( rd rs1 shamt -- )        00 1 13 `instr/i/shift ;
: `srli  ( rd rs1 shamt -- )        00 5 13 `instr/i/shift ;
: `srai  ( rd rs1 shamt -- )        20 5 13 `instr/i/shift ;
\ RV64 instructions.
: `lwu   ( rd rs1 imm   -- )           6 03 `instr/i ;
: `ld    ( rd rs1 imm   -- )           3 03 `instr/i ;
: `addiw ( rd rs1 imm   -- )           0 1B `instr/i ;
: `slliw ( rd rs1 shamt -- )        00 1 1B `instr/i/shift ;
: `srliw ( rd rs1 shamt -- )        00 5 1B `instr/i/shift ;
: `sraiw ( rd rs1 shamt -- )        20 5 1B `instr/i/shift ;

\ S-type instructions.
: `instr/s ( rs2 rs1 offset fn3 op -- )
  [ % % % % v v v v ]  ( -- op rs2 rs1 offset fn3 )
  [ % % %     v v v ]  ( -- op fn3 rs2 rs1 offset )
  DUP 4 0 [:]          ( -- op fn3 rs2 rs1 offset imm5 )
  [ % % % % v v v v ]  ( -- op imm5 fn3 rs2 rs1 offset )
  B 5 [:]              ( -- op imm5 fn3 rs2 rs1 imm7 )
  [ ^ %         v v ]  ( -- op imm5 fn3 rs1 rs2 imm7 )
  `instr/rsb ;
: `sb  ( rs2 rs1 offset -- )   0 23 `instr/s ;
: `sh  ( rs2 rs1 offset -- )   1 23 `instr/s ;
: `sw  ( rs2 rs1 offset -- )   2 23 `instr/s ;
: `sd  ( rs2 rs1 offset -- )   3 23 `instr/s ;

\ B-type instructions.
: `instr/b ( rs1 rs2 offset fn3 op -- )
  [ % % % % v v v v ]   ( -- op rs1 rs2 offset fn3 )
  [ % % %     v v v ]   ( -- op fn3 rs1 rs2 offset )
  DUP DUP  4 1 [:]      ( -- op fn3 rs1 rs2 offset offset offset[4:1] )
  1 << SWAP  B B [:] |  ( -- op fn3 rs1 rs2 offset imm5 )
  [ % % % % v v v v ]   ( -- op imm5 fn3 rs1 rs2 offset )
  DUP  C C [:]		( -- op imm5 fn3 rs1 rs2 offset offset[12] )
  6 << SWAP  A 5 [:] |  ( -- op imm5 fn3 rs1 rs2 imm7 )
  `instr/rsb ;
: `beq  ( rs1 rs2 offset -- )   0 63 `instr/b ;
: `bne  ( rs1 rs2 offset -- )   1 63 `instr/b ;
: `blt  ( rs1 rs2 offset -- )   4 63 `instr/b ;
: `bge  ( rs1 rs2 offset -- )   5 63 `instr/b ;
: `bltu ( rs1 rs2 offset -- )   6 63 `instr/b ;
: `bgeu ( rs1 rs2 offset -- )   7 63 `instr/b ;

\ Common format for U/J-type instructions.
: `instr/uj  ( op rd imm20 -- )
  5 << | 7 << | ` ;

\ U-type instructions.
: `instr/u ( rd imm opcode -- )
  ROT ROT `instr/uj ;
: `lui   ( rd imm -- )   37 `instr/u ;
: `auipc ( rd imm -- )   17 `instr/u ;

\ J-type instructions.
: `instr/j ( rd offset op -- )
  ROT ROT               ( -- op rd offset )
  DUP 13 0C [:] SWAP    ( -- op rd offset[19:12] offset )
  DUP 0B 0B [:] SWAP    ( -- op rd offset[19:12] offset[11] offset )
  DUP 0A 01 [:] SWAP    ( -- op rd offset[19:12] offset[11] offset[10:1] offset )
      14 14 [:]		( -- op rd offset[19:12] offset[11] offset[10:1] offset[20] )
  A << | 1 << | 8 << |  ( -- op rd imm20 )
  `instr/uj ;
: `jal ( rd offset -- )  6F `instr/j ;

\ ABI names for registers.
: zero 00 ;   : s0   08 ;   : a6  10 ;   : s8  18 ;
: ra   01 ;   : s1   09 ;   : a7  11 ;   : s9  19 ;
: sp   02 ;   : a0   0A ;   : s2  12 ;   : s10 1A ;
: gp   03 ;   : a1   0B ;   : s3  13 ;   : s11 1B ;
: tp   04 ;   : a2   0C ;   : s4  14 ;   : t3  1C ;
: t0   05 ;   : a3   0D ;   : s5  15 ;   : t4  1D ;
: t1   06 ;   : a4   0E ;   : s6  16 ;   : t5  1E ;
: t2   07 ;   : a5   0F ;   : s7  17 ;   : t6  1F ;


\ --- Common FORTH words --- \

\ Arithmetic operators.
: NEGATE ( n -- n' )      [ >t0 ]       [ t0 zero t0 `sub ] [ t0> ] ;
: +      ( n1 n2 -- n )   [ >t1 ^ >t0 ] [ t0   t0 t1 `add ] [ t0> ] ;
: *      ( n1 n2 -- n )           ;
: /MOD   ( n1 n2 -- rem quot )    ;
: /      ( n1 n2 -- quot )        ;
: MOD    ( n1 n2 -- rem )         ;
: */     ( n1 n2 n3 -- n )        ;
: */MOD  ( n1 n2 n3 -- rem quot ) ;
: 1+     ( n -- n' )     [ >t0 ]       [ t0 t0   1 `addi ] [ t0> ] ;
: 1-     ( n -- n' )     [ >t0 ]       [ t0 t0 FFF `addi ] [ t0> ] ;
: LSHIFT ( n u -- n' )   [ >t1 ^ >t0 ] [ t0 t0  t1 `sll  ] [ t0> ] ;
: RSHIFT ( n u -- n' )   [ >t1 ^ >t0 ] [ t0 t0  t1 `srl  ] [ t0> ] ;
: 2*     ( n -- n' )     [ >t0 ]       [ t0 t0   1 `slli ] [ t0> ] ;
: 2/     ( n -- n' )     [ >t0 ]       [ t0 t0   1 `srai ] [ t0> ] ;
: ABS    ( n -- n' )     [ >t0   >t1 ] [ t1 t1  3F `srai ]            \ mask@t1 >>= 63
				       [ t0 t0  t1 `xor  ]            \ t0 = t0 xor mask@t1
				       [ t0 t0  t1 `sub  ] [ t0> ] ;  \ t0 -= mask@t1
: MIN    ( n1 n2 -- nmin )   2DUP -          ( -- n1 n2 n1-n2 )
			     DUP ABS         ( -- n1 n2 n1-n2 |n1-n2| )
			     - 2/ +          ( -- n1 nmin )
			     SWAP DROP ;     ( -- nmin )
: MAX    ( n1 n2 -- n )   2DUP -  DUP ABS  + 2/ +  SWAP DROP ;

\ Logic (bitwise) operators.
: FALSE ( -- false )    0 ;
: TRUE  ( -- true )     [ t0 zero FFF `addi ] [ v t0> ] ;
: AND ( x1 x2 -- x )    & ;
: OR  ( x1 x2 -- x )    | ;
: XOR ( x1 x2 -- x )    [ >t1 ^ >t0 ] [ t0 t0  t1 `xor  ] [ t0> ] ;
: INVERT ( x -- x' )    [ >t0 ]       [ t0 t0 FFF `xori ] [ t0> ] ;

\ Comparison operators.
: t0<>0   t1 zero  t0 `sub
	  t0   t0  t1 `or
	  t0   t0  3F `srai ;
: 0<> ( n -- flag )       [ >t0 ]                          [ t0<>0 ] [ t0> ] ;
: 0=  ( n -- flag )       0<> INVERT ;
: <>  ( n1 n2 -- flag )   [ >t1 ^ >t0 ] [ t0 t0 t1 `xor  ] [ t0<>0 ] [ t0> ] ;
: =   ( n1 n2 -- flag )   <> INVERT ;
: <   ( n1 n2 -- flag )   [ >t1 ^ >t0 ] [ t0 t0 t1 `slt  ] [ t0<>0 ] [ t0> ] ;
: >   ( n1 n2 -- flag )   [ >t1 ^ >t0 ] [ t0 t1 t0 `slt  ] [ t0<>0 ] [ t0> ] ;
: U<  ( u1 u2 -- flag )   [ >t1 ^ >t0 ] [ t0 t0 t1 `sltu ] [ t0<>0 ] [ t0> ] ;
: U>  ( u1 u2 -- flag )   [ >t1 ^ >t0 ] [ t0 t1 t0 `sltu ] [ t0<>0 ] [ t0> ] ;
: 0<  ( n -- flag )       0 < ;
: 0>  ( n -- flag )       0 > ;

\ Memory access.
: C! ( c addr -- )   [ >t1 ^ >t0 ^ ] [ t0 t1  0 `sb  ]         ;
: C@ ( addr -- c )   [ >t1         ] [ t0 t1  0 `lbu ] [ t0> ] ;
: !  ( n addr -- )   [ >t1 ^ >t0 ^ ] [ t0 t1  0 `sd  ]         ;
: @  ( addr -- n )   [ >t1         ] [ t0 t1  0 `ld  ] [ t0> ] ;
: +! ( n addr -- )   DUP @  ( -- n addr n0 )
		     ROT +  ( -- addr n' )
		     SWAP ! ;


\ --- Global variables --- \

: STATE ( -- addr )    [ t0 s2 20 `ld ] [ v t0> ] ;
: LATEST ( -- addr )   [ t0 s2 28 `ld ] [ v t0> ] ;


\ --- Data space --- \

: HERE   ( -- addr )   [ t0 s1  0 `addi ] [ v t0> ] ;
: HERE!  ( addr -- )   [ >t0 ^ ] [ s1 t0  0 `addi ] ;

: ALLOT ( n -- )   [ >t0 ^ ] [ s1 s1 t0 `add ] ;
: CHARS ( n -- n' )            ;
: CELLS ( n -- n' )   3 LSHIFT ;

: , ( n -- )
  1 CELLS ALLOT
  [ >t0 ^ ] [ t0 s1  8 NEGATE  `sd ] ;
: C, ( n -- )   . ;


\ --- Defining words --- \

: IMMEDIATE ( -- )
  LATEST 10 +			( -- flag-addr )
  DUP C@  80 |			( -- flag-addr flag' )
  SWAP C! ;

: LITERAL ( C: x -- ) ( -- x )
  \ Compile "lui t0, 0xHHHHH[+1]".
  DUP  ( mask: ) FF 4 LSHIFT F |  &  ( -- n low )
  DUP B RSHIFT ROT		     ( -- low sign n )
  C RSHIFT +  t0 SWAP  `lui	     ( -- low )
  \ Compile "addi t0, 0xLLL".
  t0 t0 ROT  `addi                   ( -- )
  \ Compile a sequence that pushes `t0` on the stack.
  v t0> ;

: create ( "<spaces>name" -- )
  pname
  [ v ] [ s0 s3  0 `sd   ]	   \ save INPUT@s0
	[ s0 a0  0 `addi ]	   \ INPUT@s0 = addr@a0
	[ a0 s2 28 `ld   ]	   \ latest@a0 = [LATEST] (as required by Head)
  Head  [ s0 s3  0 `ld   ] [ ^ ] ; \ restore INPUT@s0

: CREATE ( "<spaces>name" -- ) ( -- addr )
  create
  \ Compile code that pushes on the stack the address of the empty
  \ space following the CREATEd definition.
  t0 0 `auipc
  t0 t0 14 `addi
  v t0>
  \ Compile "jalr zero, 0(ra)".
  zero ra 0 `jalr ;

: VARIABLE ( "<spaces>name" -- ) ( -- addr )   CREATE  0 , ;
: CONSTANT ( x "<spaces>name" -- ) ( -- x )    create  LITERAL  zero ra 0 `jalr ;


\ --- Return stack management --- \

: >>t0 ( R: n -- n )    t0   sp 0        `ld   ;
: t0>> ( R: ? -- n )    t0   sp 0        `sd   ;
: ^^   ( R: n -- .. )   sp   sp 8        `addi ;
: vv   ( R: .. -- n )   sp   sp 8 NEGATE `addi ;
: ret  ( -- )           zero ra 0        `jalr ;

: >R ( x --   ) ( R:   -- x )   [ 8 NEGATE ALLOT ] [  >t0  ^ ] [ vv t0>> ] [ ret ] ;
: R> (   -- x ) ( R: x --   )   [ 8 NEGATE ALLOT ] [ >>t0 ^^ ] [  v t0>  ] [ ret ] ;
: R@ (   -- x ) ( R: x -- x )   [ 8 NEGATE ALLOT ] [ >>t0    ] [  v t0>  ] [ ret ] ;


\ --- Control flow --- \

\ IF ... ELSE ... THEN
: IF ( C: -- orig ) ( flag -- )
  \ Syntax: flag IF ... ELSE ... THEN

  \ Run-time:
  \ - pop a flag off the stack into a register:
  \   -> ">t0 ^"
  \ - branch based on the register value (offset currently unknown):
  \   -> "beq t0, zero, +???"
  >t0 ^				 \ t0 = flag                       ( -- )
  t0 zero 0 `beq		 \ if flag@t0 = 0:
				 \   goto +???.                    \ To be backpatched by ELSE/THEN.
  \ Compilation:
  \ - push address of the branch instruction on the stack;
  \   - this address is backpatched by the corresponding ELSE/THEN.
  HERE 4 -                       \ push HERE-4 (`beq` is previous instruction)  ( C: -- orig )
  ; IMMEDIATE

: resolve ( C: orig -- ) ( -- )
  \ Compilation:
  \ - backpatch branch instruction at `orig` to use offset `HERE - orig`.
  \ - NOTE: this could be simplified if the assembler supported
  \   writing instructions on the stack rather than at `OUTPUT`.
  HERE                           \ save OUTPUT@s1                  ( C: -- orig OUTPUT )
  OVER HERE!                     \ OUTPUT@s1 = orig
  DUP ROT -                      \ offset = OUTPUT - orig          ( C: -- OUTPUT offset )
  t0 zero ROT  `beq              \ compile "beq t0, zero, offset"  ( C: -- OUTPUT )
  HERE! ;                        \ restore OUTPUT@s1               ( C: -- )

: ELSE ( C: orig -- orig' ) ( -- )
  \ Run-time:
  \ - jump forward unconditionally (offset currently unknown):
  \   -> "addi t0, zero, 0
  \   -> "beq t0, zero, +???"
  t0 zero 0 `addi                \ t0 = 0
  t0 zero 0 `beq                 \ if t0 = 0:  \ Always true!
				 \   goto +???.                    \ To be backpatched by THEN.

  \ Compilation:
  \ - backpatch branch instr. at `orig` to jump HERE (same as THEN);
  \ - push address of the (unconditionalized) branch on the stack;
  \   - this address is backpatched by the corresponding THEN.
  resolve
  HERE 4 -                       \ push HERE-4 (`beq` is previous instruction)  ( C: -- orig' )
  ; IMMEDIATE

: THEN ( C: orig -- ) ( -- )
  \ Compilation:
  resolve ; IMMEDIATE

\ BEGIN ... WHILE ... REPEAT
: BEGIN ( C: -- dest ) ( -- )
  \ Compilation:
  HERE ; IMMEDIATE

: WHILE ( C: dest -- orig dest ) ( flag -- )
  \ Append run-time semantics to current definition:
  >t0
  ^
  t0 zero BAD `beq

  \ Compilation:
  HERE 4 - SWAP ; IMMEDIATE

: REPEAT ( C: orig dest -- ) ( -- )
  \ Run-time:
  HERE -  			( C: orig offset )
  zero SWAP  `jal

  \ Compilation:
  resolve ; IMMEDIATE


\ --- I/O --- \

: TYPE ( c-addr u -- )
  [ a1 s3 0 `ld ] [ ^ ]
  [ a0 s3 0 `ld ] [ ^ ]
  TYPE ;
: EMIT ( c -- )
  [ t0 s3 0 `addi ] [ v t0> ]	( -- c c-addr )
  1 TYPE  DROP ;
: CR ( -- )       0A EMIT ;
: SPACE  ( -- )   20 EMIT ;
: SPACES ( n -- )
  BEGIN DUP 0> WHILE
    SPACE
    1-
  REPEAT ;
: . ( n -- )
  0 >R  >R			( -- ) ( R: -- count n )
  BEGIN				( -- digit* )
    R@ F AND			( -- digit* digit )
    R> 4 RSHIFT			( -- digit* digit n' ) ( R: -- count )
    R> 1+ >R			( -- digit* digit n' ) ( R: -- count' )
    DUP >R			( -- digit* digit n' ) ( R: -- count' n' )
    0 U>			( -- digit* digit flag )
  WHILE REPEAT			( -- digit* digit ) ( R: -- count' n' )
  R> DROP 			( R: -- count )
  BEGIN				( -- digit* digit ) ( R: -- count )
    DUP  9 > IF			( -- digit* digit letter? )
      7 +
    THEN  30 +			( -- digit* char )
    EMIT			( -- digit* )
    R> 1- DUP >R		( -- digit* count' ) ( R: -- count' )
    0>				( -- digit* flag )
  WHILE REPEAT			( -- digit* ) ( R: -- count' )
  R> DROP  SPACE ;		( R: -- )

