\ --- FizzBuzz --- \

CREATE Fizz   46 C, 69 C, 7A C, 7A C,
CREATE Buzz   42 C, 75 C, 7A C, 7A C,

: MOD ( n1 n2 -- n )
  SWAP				( -- n2 n1 )
  BEGIN 2DUP -			( -- n2 n1 diff )
	DUP 0<			( -- n2 n1 diff flag1 )
	SWAP 0=			( -- n2 n1 flag1 flag2 )
	OR			( -- n2 n1 flag )
  WHILE				( -- n2 n1 )
    OVER -			( -- n2 n1' )
  REPEAT
  SWAP DROP ;          		( -- n1' )

: FizzBuzz
  64 0 BEGIN			( -- limit i )
    2DUP >			( -- limit i flag )
  WHILE				( -- .. i )
    1+  FALSE			( -- .. i' flag )
    OVER  3 MOD 0=		( -- .. i' flag flag/3 )
    DUP IF
      Fizz 4 TYPE		\ Print "Fizz".
    THEN OR			( -- .. i' flag' )
    OVER  5 MOD 0=		( -- .. i' flag' flag/5 )
    DUP IF
      Buzz 4 TYPE		\ Print "Buzz".
    THEN OR			( -- .. i' flag'' )
    0= IF			( -- .. i' )
      DUP .			\ Otherwise print the number.
    THEN 			( -- .. i' )
    CR				\ Print newline.
  REPEAT ;

FizzBuzz BYE
