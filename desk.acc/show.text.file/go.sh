#!/bin/bash

set -e

CC65=~/dev/cc65/bin

original=show_text_file.bin
disasm=stf.d

src=stf.s
obj=stf.o
list=stf.list
out=stf

# Origin of STF
#echo '        .org $800' > $disasm

# Disassemble original source
#$CC65/da65 $original --info show_text_file.info >> $disasm

#cp $disasm $src

# Assemble
$CC65/ca65 --target apple2enh --listing $list -o $obj $src

# Link
$CC65/ld65 --config apple2-asm.cfg -o $out $obj

# Verify original and output match
diff $original $out

# Show output for review
less $list