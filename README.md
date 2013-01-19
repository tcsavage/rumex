rumex
=====

bRainfUck iMproved EXtended - An extended version of the RUM Brainfuck variant implemented in Haskell

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
  * Any natural number - Write the value of the integer to the tape
  * `"string"` - Append string to the input
  * `(commands)` - Create a stored procedure containing `commands`, with an ID equal to the current value on the tape
  * `:` - Invoke stored procedure with the ID equal to the current value on the tape

*RUMEX*
  * `#` - Write current value to console as a number
  * `{* ... *}` - Explicit comment blocks
