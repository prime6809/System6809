#

all: acorn6809 acorn6809h

acorn6809: acorn6809.ic4.asm
	lwasm --raw -o acorn6809.rom --list=acorn6809.txt --symbols acorn6809.ic4.asm

acorn6809h: acorn6809.ic4.asm
	lwasm --raw -o acorn6809-high.rom --list=acorn6809-high.txt -D AUTO -D VDU80 -D HIGH acorn6809.ic4.asm

clean:
	rm -f *.rom
	rm -f *.txt
	