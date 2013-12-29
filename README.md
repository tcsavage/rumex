RUMEX
=====

bRainfUck iMproved EXtended - An extended version of the RUM Brainfuck variant implemented in Haskell.

This project serves as both an educational tool - for leraning about lexers, parsers, interpreters and compilers - and as a fun toy language to play with.

RUMEX is a modified implementation of the RUM Brainfuck variant. Where as RUM is a superset of Brainfuck, RUMEX is not. This is because RUMEX uses explicit comment delimiters, as opposed to Brainfuck's "anything that isn't code is ignored" approach. This is primarily to permit comments to contain characters which would otherwise be considered code, but it has the added advantage of improving clarity as well.

Commands
--------

*Standard Brainfuck*
  * `+` - Increment current cell
  * `-` - Decreement current cell
  * `<` - Move pointer left
  * `>` - Move pointer right
  * `.` - Write current value to console as ASCII
  * `,` - Read character from input and store ASCII value
  * `[` - Jump to matching `]` if current value is zero
  * `]` - Jump to matching `[` if current value is non-zero

*RUM*
  * `nOp` - Repeat `Op` `n` times
  * `"string"` - Append string to the input
  * `(commands)` - Create a stored procedure containing `commands`, with an ID equal to the current value on the tape
  * `:` - Invoke stored procedure with the ID equal to the current value on the tape

*RUMEX*
  * `#` - Write current value to console as a number
  * `{* ... *}` - Explicit comment blocks
