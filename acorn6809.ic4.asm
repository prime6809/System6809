;
; Acorn 6809 board monitor rom.
; Original source copyright 1980 Acorn Computer Ltd.
; 
; Converted to source prepared to assemble with lwasm.
; 2018-09-17 Phill Harvey-Smith.
;
; Requires lwasm 5.0beta or better to assemble.
;
;
; Two memory maps are supported by this rom, the original 6809 card
; which had the periperals in block 0 like the 6502 card, and the later 
; version which moved them all up to high memory, to allow a mostly RAM
; machine for running Flex / OS-9.
;
; This is controlled by the value of HIGH, if undefinedd the original memory map
; is selected, if defined the high memory map is selected.
;
; periperal			Original Addresses	High Addresses
; 6809 card RAM	$	0000-$0400			$E000-$E400
;
; 6809 card VIA		$0980-$098F			$E980-$E98F
; 
; 40col RAM			$0400-$07FF			$E400-$E7FF
; 40col CRTC		$0800-$0801 		$E800-$E801
;
; 80col RAM			$1000-$17FF			$F000-$F7FF
; 80col CRTC		$1840-$1841 		$E840-$E841
;
; 8271 Floppy		$0A00-$0A0F			$EA00-$EA0F
;
; Note though the onboard RAM moves to $E000 for the HIGH variant, the 
; card can still be run stand alone, as unlike the 6502, the stack can be
; anywhere in the memory map, and the direct page can be any of the 
; 256 pages of ram rather than being fixed at 0 as with the 6502 zero page.
;
; I suspect that most of the above remapping is done by changing the decoding 
; PROM on the card and arranging for the nBLOCK0 line on the eurocard 
; connector to be active in the $E000 block instead of $0000.
;
; The version of the rom dumped from the system 3 at the Centre for 
; computing history has several mods including using an 80 column VDU card.
; Later versions of the schematics for this card also had jumpers to select
; addressing (as detailed above).
; 
; The version of the CRTC code selected is controlled by VDU80 when defined,
; then the 80 column code is selected, any other value selects the 40 
; column code.
;
; The above ROM dump also contains code to auto boot from disk without user 
; intervention the assembly of this is controlled by defining AUTO
;
; This program handles a memory mapped vdu, encoded keyboard
; cassette interface, parallel printer and mini-floppy bootstrap
;


	ifdef HIGH
BASEPG	equ		$E0
BOOTADR	equ		$C000
	else	
BASEPG	equ		$00
BOOTADR	equ		$0000
	endc
	
BASE	equ		BASEPG*256

MPROM	equ		$f800			; Normal monitor position at top of memory
MONDP	equ		BASEPG+$03		; direct page for monitor
MONDPAD	equ		MONDP*256		; Monitor DP address

	ifdef VDU80
ROWS	equ		25				; number of rows in display
COLS	equ		80				; number of colums in display
PSIZE	equ		2048			; size of display memory
WRAPMSK	equ		$07				; Wrap around mask

PAGE	equ		BASE+$1000		; location of memory that VDU uses		
CRTC	equ		BASE+$0840		; location of CRTC controller on VDU card
	else
ROWS	equ		25				; number of rows in display
COLS	equ		40				; number of colums in display
PSIZE	equ		1024			; size of display memory
WRAPMSK	equ		$03				; Wrap around mask

PAGE	equ		BASE+$0400		; location of memory that VDU uses		
CRTC	equ		BASE+$0800		; location of CRTC controller on VDU card
	endc
	
PAGHI	equ		12				; Page address register high byte
CURHI	equ		14				; cursor address register high byte

DRIVE	equ		$40				; bootstrap drive, other drive is $80
FLOPY	equ		BASE+$0A00		; Address of 8271 floppy controller
FDCC	equ		FLOPY+0			; Command register
FDCS	equ		FDCC			; status register
FDCP	equ		FLOPY+1			; Parameter register
FDCR	equ		FDCP			; result register
FDRST	equ		FLOPY+2			; reset register
FDCD	equ		FLOPY+4			; data register

KVIA	equ		BASE+$0980		; location of VIA on CPU board
KORB	equ		KVIA+0			; output register b
KIRB	equ		KORB			; input register b
KORA	equ		KVIA+1			; output register a
KIRA	equ		KORB			; input register a
KDDRB	equ		KVIA+$2			; DDR b
KDDRA	equ		KVIA+$3			; DDR a
KT1CL	equ		KVIA+$4			; timer 1 low
KT1CH	equ		KVIA+$5			; timer 1 high
KT1LL	equ		KVIA+$6			; timer latch 1 low
KT1LH	equ		KVIA+$7			; timer latch 1 high
KT2CL	equ		KVIA+$8			; timer 2 low
KT2CH	equ		KVIA+$9			; timer 2 high
KSR		equ		KVIA+$A			; shift register
KACR	equ		KVIA+$B			; aux control register
KPCR	equ		KVIA+$C			; peripheral control register
KIFR	equ		KVIA+$D			; interrupt flag register
KIER	equ		KVIA+$E			; interrupt enable register
KORA2	equ		KVIA+$F			; i/o register without handshake

INTDEL	equ		15*256			; delay for single instruction trace
T1IFLG	equ		%01000000		; interrupt flag position for timer 1
CB1FLG	equ		%00010000		; interrupt flag position for keyboard interrupt
IKPCR	equ		%11101111		; Initial value for periperal control register
								; b0 	+ve edge printer interrupt on ca1
								; 		but not used in this monitor
								; b1-b3 output to activate printer, normally high
								; b4 	-ve edge keyboard interrupt
								; b5-b7	cassette output initially set high
IKIER	equ		%11010000		; interrupt enable register control
								; b4	Keyboard interrupt enable
								; b6 	Timer 1 interrupt enable
								; b7	Set interrupts enabled
								; other enables not altered
PSTRB	equ		%00000010		; bit position of printer strobe in pcr
COPBIT	equ		%00100000		; bit position that controlls caeeette output in pcr
SWIINS	equ		$3F				; SWI instruction uses as breakpoint in monitor
BUFLEN	equ		80+2			; buffer up to 80 characters for keyboard
PROMPT	equ		'*				; monitor prompt character
RUBCH	equ		$7F				; keyboard char that does rubout
BSPACE2	equ		$08				; vdu character that does backspace / rubout
BSPACE	equ		$7F				; vdu character that does backspace / rubout
LF		equ		$0A				; linfeed
CR		equ		$0D				; carrage return
SPACE	equ		$20				; space char
COMMA	equ		',				; comma char
MINUS	equ		'-				; Minus char
FFEED	equ		$0C				; formfeed used as Clear screen
SEMIC	equ		';				; semicolon					
; CC flag masks?
IRQ		equ		$10
ZERO	equ		$04

NOBREAK	equ		$FFFF			; flag for no breakpoint


;
; CRTC parameters copied here by high version boot code
;
	ifdef HIGH
		org		MONDPAD
HTOTAL	RMB		1				; Horizontal total (characters)
HDISP	RMB		1				; Horizontal displayed (characters)
HSPOS	RMB		1				; H sync position
HSWIDTH	RMB		1				; H sync width	
VTOTAL	RMB		1				; Vertical total
VSCAN	RMB		1				; Vertical scan line adjust 
VSYNC	RMB		1				; V sync position
ILACE	RMB		1				; Interlace mode
MAXSCAN	RMB		1				; Max scan line address
CSTART	RMB		1				; Cursor start
CEND	RMB		1				; Cursor end
SADDRH	RMB		1				; Start address (high)
SADDRL	RMB		1
COLS2	RMB		1				; Column count / 2
	endc		

;
; this table copied from rom on start up
;
		org		BASE+$035B
ISTACK	equ		*				; initial stack starts here
RTAB1	equ		*				; ram table 1 starts here	
STACK 	RMB		2				; positon of stack pointer when empty
NTRACE 	RMB		2				; number of instructions to trace before stopping
BSECHO 	RMB		1				; character sent to backspace display
ECHOF 	RMB		1				; keyboard buffer/echo control
								; bits 0-5, don't cares
								; bit 6	echo CON5ole input to CON5ole output if set
								; bit 7	buffer input lines, allow rubout if set
								
PFLAG 	RMB		1				; printer control flag, echo CON5ole output to printer if
PNEW 	RMB		1				; this character not sent to printer	non-zero
DELCNT 	RMB		2				; delay count for cassette, controls baud rate
COPADR 	RMB		2				; address for CON5ole output
CINADR 	RMB		2				; address for CON5ole input
CASOPA 	RMB		2				; address for cassette output
CASINA 	RMB		2				; address for cassette input
PRINTI 	RMB		2				; address of printer output routine
FUNCTI 	RMB		2				; address of vdu function table
CMNDI 	RMB		2				; address of monitor command table
IRQRTS 	RMB		2				; address to go to on timer 1 interrupt
LINEPT 	RMB		2				; address of memory input line. 0 if none
IRESV 	RMB		2				; address of reserved vector routine
ISWI3 	RMB		2				; address of swi3 routine
ISWI2 	RMB		2				; address of swi2 routine
IFIRQ 	RMB		2				; address of firq routine
IIRQ 	RMB		2				; address of irq routine
ISWI	RMB		2				; address of swi routine
INMI 	RMB		2				; address of nmi routine
OFFSET 	RMB		2				; cassette load offset
;
; general variables for monitor use
;


HEADST 	RMB		2				; static head pointer into line buffer
HEADDY 	RMB		2				; dynamic head pointer into line buffer
TAIL 	RMB		2				; tail pointer into line buffer
MSTACK 	RMB		2				; stack saved whilst memory interpreting
CROW 	RMB		1				; current row of cursor on display
CCOL 	RMB		1				; current column of cursor on display
CPAGE 	RMB		2				; current start of display page in memory
MSAV 	RMB		2				; saved address for memory command
GSAV 	RMB		2				; saved address for go command
NAME 	RMB		6				; saved name for cassette input/output
CSSTRT 	RMB		2				; saved cassette output start address
CSEND 	RMB		2				; saved cassette output end address
ONLINE 	RMB		1				; flag set to zero when find cr in input line
LASTC 	RMB		1				; saved last character from input line
CBREAK 	RMB		2				; current address of a breakpoint, $FFFF if none
NBREAK 	RMB		2				; number of breakpoints to ignore before stopping user
CINST 	RMB		1				; user instruction at breakpoint address
CTRACE 	RMB		2				; number of instructions left to trace before stopping us
USRSTK 	RMB		2				; saved user stack pointer when user halted
TEMP 	RMB		2				; temporary storage
BUFFER 	RMB		BUFLEN			; line input buffer

        org     MPROM
RESET
        LDA     #MONDP			; setup direct page
        TFR     A,DP
		SETDP	MONDP			; tell assembler
		
        LDX     #PTAB1			; rom table start
        LDU     #RTAB1			; ram table start
RST1
        LDA     ,X+				; copy rom
        STA     ,U+				; to ram
        CMPX    #PTAB2			; until end of table
        BNE     RST1

        LDS     <STACK			; setup stack pointer

	ifdef AUTO
		LBSR	BOOT1
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP
		NOP	
	else
        LDX     MPROM-2			; check for extra ROM
        CMPX    #$A55A
        BNE     STRT1			; not there : skip

        JSR     [MPROM-4]		; call extra ROM init routine
	endc
STRT1
        LDU     #BACK			; put monitor return onto stack
        PSHS    U
        LDD     #$000C			; put dummy registers onto stack
STRT2
        PSHS    A				
        DECB
        BNE     STRT2

        STS     <USRSTK			; save stack pointer
        LDX     #BUFFER			; get start of buffer
        STX     <HEADDY			; and setup pointers
        STX     <HEADST
        STX     <TAIL
        LBSR    CRTCI			; initialize CRTC

        LBSR    VIA1			; initialize VIA

        CLR     <LASTC			; set no saved character
        LBSR    BRKOUT			; remove if exists

        LDX     #NOBREAK		; then set non-existing
	ifdef AUTO
		JMP		MCASIN			; replaced with auto boot code
		fcb		$EF
	else
		STX     <CBREAK		
        ANDCC   #$EF			; allow interrupts
	endc
    
MON
        LDA     #PROMPT			; send prompt			
        LDX     >LINEPT			; unless memory input
        BNE     MON1

        LBSR    CONOUT			; send prompt

MON1
        STA     >ONLINE			; set no CR yet
PARSE
        LDA     #MONDP			; set monitor direct page
        TFR     A,DP
		SETDP	MONDP
		
        LDX     <LINEPT			; see if memory input
        BEQ     PARSEC			; if so then

        LDA     ,X				; see if null and end if so
        BEQ     MEND

PARSEC
        BSR     CONCHR			; get input
	
        BEQ     MON				; prompt on CR

        LDX     <CMNDI			; else command table
        LBSR    DISPCH			; search command table

        BRA     PARSE			
*
*	enter here for monitor to use memory input line
*	(x) is start of line, ends with a null.
*	multiple input lines are allowed, each line ends
*	with a carriage return
*	exits with (a) zero if all ok, else (a) is OFF
*	if found null too early, else (a) is character
*	causing error.

		SETDP	0

MEMUSE  PSHS    DP				
        STS     MSTACK			; save stack for return
        STX     LINEPT			; save pointer in memory
        CLR     LASTC			; set none saved
        BRA     MON				; and call monitor	

CONCHR  LDA     #CR				; assume CR for now
        TST     ONLINE			; and if found CR then
        BEQ     CON1			; is correct

        BSR     CONIN			; get input

        CMPA    #CR				; if not CR then done
        BNE     CON2

CON1
        CLR     ONLINE			; set found CR
CON2
        RTS
*
*	CON5ole input routine, gets character from keyboard
*	buffer or memory. if finds null in memory then returns to
*	to caller with error OFF
*
		SETDP MONDP
CONIN
        PSHS    X,DP		
        LDA     #MONDP			; setup direct page
        TFR     A,DP
        LDA     <LASTC			; saved one ?
        BEQ     CON5			; none saved

        CLR     <LASTC			; flag not saved anymore
		
        PULS    PC,X,DP     	; restore and return

CON5
        LDX     <LINEPT			; see if mem input
        BEQ     CON3			; no : use buffer	

        LDA     ,X+				; get mem value
        BNE     CON4			; ok if not null	
        DECA					; set to $FF

MEND    LDS     <MSTACK			; restore stack
        CLR     <LINEPT			; clear mem input
        CLR     <LINEPT+1
        PULS    PC,DP           ; return to caller
		
CON3    JSR     [CINADR]		; get console input

CON4    STX     <LINEPT			; store new mem pointer
        PULS    PC,X,DP         ; restore and return

*
*	this routine prints the registers from the stack as pointed
*	to by usrstk, then prints usrstk itself.
*	also prints 5 bytes starting at (pc)
*
EXREG
        LDX     #TITLES			; print headings
        BSR     STRING

        LDU     <USRSTK
        LDB     #$04			; 4x 8 bit regs : CC,A,B,DP 
EX1
        LDA     ,U+				; get reg value
        LBSR    OPARSP			; output as 2 hex digits

        DECB					; dec reg count
        BNE     EX1				; loop until all 4 done

        LDB     #$04			; 4x 16 bit regs : X,Y,U,PC
EX2
        LDX     ,U++			; get reg value
        LBSR    OPXREG			; output as 4 hex bytes

        DECB					; dec reg count
        BNE     EX2				; loop until all 4 done

        LDX     <USRSTK			; output user stack pointer
        LBSR    OPXREG			; as 2 hex digits

        LDU     10,X			; pick up user PC value
        LDX     #PCMESS			; send title
        BSR     STRING

        LDB     #$05			; then dump 5 bytes at PC
EX3
        LDA     ,U+				; get byte
        LBSR    OPARSP			; send as 2 hex digits

        DECB					; decrement count
        BNE     EX3				; loop until all done

*
* output the string cr, if to console
*

OPCRLF  LDX     #SCRLF

*
*	output the string pointed to by (x> until a null,
*	leaves x pointing to null+1, other registers intact
*

STRING
        PSHS    A
STRNG1
        LDA     ,X+				; get data from string
        BEQ     EOTXT			; end if null
        LBSR    CONOUT			; output to console
        BRA     STRNG1			; loop again

EOTXT   PULS    PC,A            ; restore and return

*
*	calculate apparent address of cursor without
*	allowing for memory wrap around,. result in d
*
CCOFST  
	ifdef HIGH
		LDA		<HDISP			; get cols / line
	else
		LDA     #COLS			; get cols / line
	endc
        LDB     <CROW			; get current row
        MUL						; multiply them
        ADDB    <CCOL			; add current column
        ADCA    #$00
        ADDD    <CPAGE			; add current page
        RTS
*
*	calculate real address of cursor in memory space,
*	result returned in x
*
		
CCLOCN  PSHS    D				; save regs					
        BSR     CCOFST			; get apparent address

	    ANDA    #WRAPMSK		; wrap arround in 1K/2K page
		ADDD    #PAGE			; add start page
        TFR     D,X				; result in X
        PULS    PC,D            ; restore and return

*
*	initialise versatile interface adapter.
*
VIA1    LDD     #(IKPCR*256)+IKIER	
        LDX     #KVIA			; point to VIA
        STA     (KPCR-KVIA),X	; peripheral control register
        STB     (KIER-KVIA),X	; interrupt control register
        LDA     #$7F			; printer output (7 bits) plus 
        STA     (KDDRA-KVIA),X	; cassette input on side A
        LDA     (KIFR-KVIA),X	; cancel any interrupts
        STA     (KIFR-KVIA),X
        RTS

*
*	initialise crt controller on vdu card, r or s version
*
CRTCI   
	ifdef HIGH
		LDX     #HTOTAL			; table for V version
    else
		LDX     #CRTCSV			; table for V version
    endc
		LDD     #((PAGHI+1)*256)+$AA	; test the page register $0DAA
        STD     CRTC			; low byte for read / write
        CMPB    CRTC+1
        BEQ     CRTCI1			; if so then v version

        LEAX    <<(CRTCRV-CRTCSV),X	; else get r table
CRTCI1  LDA     #$0B			; set up 12 registers
CRTC2   LDB     A,X				; get register data
        STD     CRTC			; write to CRTC
        DECA					; decrement reg count
        BPL     CRTC2			; loop until done
*
*	reset display to a blank page with cursor
*	at top left of screen
*
CLRALL  CLR     <CROW			; row 0
        CLR     <CCOL			; col 0
        CLR     <CPAGE			; page 0
        CLR     <CPAGE+1
		
        LBSR    SETTOP			; Set page in CRTC
        LBSR    SETCUR			; set cursor in CRTC

*
*	clear the display memory to all blanks
*
CLRSCN  LDD     #SPACE+(256*SPACE)	; Init display memory
        LDX     #PAGE			; point to it
CLRS1   STD     ,X++			; save spaces				
		CMPX    #PAGE+PSIZE		; end of screen memory?
		BNE     CLRS1			; no : keep going
        RTS

*
*	this routine used to put a character to the vdu,
*	handling cr,lf,backspace, and form feed.
*	all registers are saved
*

DISPLA  PSHS    U,Y,X,DP,B,A,CC	; save regs
        LDB     #MONDP			; setup DP
        TFR     B,DP
        LDX     <FUNCTI			; get pointer to function table
        ORCC    #IRQ			; mask IRQ
        BSR     DISPCH			; Go call function

LF967
        PULS    PC,U,Y,X,DP,D,CC	; Restore and return
		
*
*	this routine puts a character on display and moves cursor,
*

SIMCHR  BSR     CCLOCN			; find location in memory
        STA     ,X				; store the character
		INC     <CCOL			; increment cursor column
        LDA     <CCOL			; check for right hand end of line
	ifdef HIGH
        CMPA    <HDISP
    else
        CMPA    #COLS
    endc
		BNE     SIM1			; no : skip on

        CLR     <CCOL			; set column to left hand end (0)

*
* move cursor down 1 line, scroll display if required
*
DOLF    INC     <CROW			; move to next line
        LDA     <CROW			; check for end of screen?
	ifdef HIGH
		CMPA	<VSYNC
	else
        CMPA    #ROWS
	endc
        BNE     SIM1			; nope : skip on

        DEC     <CROW			; end of screen backup one line
        BSR     SCROLL			; and scroll screen up

SIM1    BRA     SETCUR			; Set new cursor pos

*
* move cursor back erasing last character
*
DORUB   BSR     BSONE			; back up cursor
        LDA     #SPACE			; print space
        BSR     DISPLA
        BSR     BSONE			; backup cursor over space
        BRA     SETCUR			; set new cursor pos

*
* back cursor up allowing line and row underflow
*

BSONE   DEC     <CCOL			; move cursor left
        BPL     BS1				; no underflow

	ifdef HIGH
		LDA		<SADDRL			; else set cursor to right margin
	else
        LDA     #COLS-1			; else set cursor to right margin
	endc
        STA     <CCOL
        DEC     <CROW			; backup one line
        BPL     BS1				; no underflow

        CLR     <CROW			; set line and coulmn to 0
        CLR     <CCOL
BS1     RTS
*
*	dispatch routine looks up 0	raster in a table,
*	character in a, table addr	, x.
*
*	table format is- first byte, number of entries 1 to 255
*
*	repeat for each entry- character to match with,
*		2 byte offset from start of this
*		table of routine to jump to if
*		characters match.
*
*		flag byte- control if no match found
*
*		positive- next word is offset of default routine
*		zero- return to calling program
*		negative- next word is address of another table to search 
*

DIS2    LDD     ,X				; get address of new table
        STD     2,S				; replace old on stack
        PULS    U,X,D			; and begin again
		
DISPCH  PSHS    U,X,D			; save registers
        LDB     ,X++			; get length and move to offset

DIS1    CMPA    -1,X			; compare character
        BEQ     DIS3			; found it

        LEAX    3,X				; else move down table
        DECB					; repeat until end of table
        BNE     DIS1

        TST     -1,X			; test flag byte
        BEQ     DIS4			; zero means return
        BMI     DIS2			; otherwise search new table 

DIS3    LDD     ,X				; get offset 
        ADDD    2,S				; add start of table
        STD     4,S				; store back on stack
        PULS    PC,X,D          ; Restore and return
		
DIS4    PULS    PC,U,X,D       	; Restore and return

*
* 	scroll the display up one line, leave cursor at same
*	position on screen,leave registers intact
*

SCROLL  LDD     <CPAGE			; get start of page
        ADDD    #(ROWS*COLS)	; then move off and
        LDX     #PAGE			; actual memory address	
        LDU     #(SPACE*256)+SPACE	; two spaces

	ifdef HIGH
		LDY		<COLS2			; number of cols to blank
		NOP
	else
        LDY     #(COLS/2)		; number of cols to blank
	endif

SCR1   	ANDA    #WRAPMSK		; Wrap arround at 1K
        STU     D,X				; put 2 blanks in memory
        ADDD    #$0002			; increment pointer
        LEAY    -1,Y			; decrement col counter
        BNE     SCR1			; keep going till all collumns done

        SUBD    #(ROWS*COLS)	; move to second line now
        ANDA    #WRAPMSK		; wrap arround and
        STD     <CPAGE			; set new page
        BSR     SETCUR			; put cursor back in position

*
*	put page address into crt controller
*
SETTOP  LDX     <CPAGE			; get page
        LDA     #PAGHI			; point to page start register
LF9EA   BRA     SETPAR			; then enter parameters

*
*	do a carriage return by setting column to 0
*
DOCR    CLR     <CCOL

*
*	set cursor register in crt controller 
*

SETCUR	LBSR    CCOFST			; get cursor address no wrap arround
        TFR     D,X				; get it into x
        LDA     #CURHI			; point at cursor register
	
*
*	put 2 byte value into crt controller, value in x,
*	high byte register number in a
*	
SETPAR  PSHS    X				
LF9F7	PULS    B				; get high byte
        STD     CRTC			; set register and data
        INCA					; move to low byte
        PULS    B				; get low byte
        STD     CRTC			; set register and data
        RTS
*
*	this routine puts keyboard input into the line buffer
*	if no room then ignores character, else echoes to display
*	if echo is switched on. handles rubout unless line buffer
*	is switched off
*

HAVCHR  LDB     <ECHOF			
LFA05   BPL     HAV3			; buffer off so no rubout

        CMPA    #RUBCH			; is it rubout?		
        BEQ     BSP1			; yep : go do it

HAV3   	BSR     PUTCHR			; put into buffer
        BEQ     PUT1			; no room : do not echo

        TSTB					; if buffer off then no linefeed on CR input
        BPL     HAV4

        CMPA    #CR				; is it a CR?
        BNE     KBECHO			; no : don't also send lf
        BSR     KBECHO			; echo CR

        LDA     #LF				; echo an LF

HAV4    STX     <HEADST			; move static pointer up

KBECHO  LDB     <ECHOF			; test for echo on?
        LSLB					; echo flag in bit 6
        BPL     PUT1			; not on if zero

*
*	console output routine, sends to printer also
*	if switched on
*
		SETDP	0
CONOUT	TST     PFLAG			; if printer off the pflag==0
        BEQ     CANOP1			

        JSR     [PRINTI]		; else send to printer

CANOP1  JMP     [COPADR]		; then to the console

		SETDP	MONDP
BSP1    LDX     <HEADDY			; end of line if
        CMPX    <HEADST			; at start of line....
        BEQ     PUT1			; so nothing to rubout....

        CMPX    #BUFFER			; do cyclic
        BNE     BSP2

        LEAX    BUFLEN,X		; decrement of pointer
BSP2    LEAX    -1,X
        STX     <HEADDY			; set new end of line
        LDA     <BSECHO			; and echo a backspace
        BRA     KBECHO
*
*	put Cnaracter into buffer if room, return z=0,
*	else return z=1
*

PUTCHR  LDX     <HEADDY			; get pointer
        BSR     BUMPU			; if there's no room then pointers equal
        CMPX    <TAIL
        BEQ     PUT1			; so done!

        STA     ,X				; store char
        STX     <HEADDY			; set new pointer
        ANDCC   #255-ZERO		; set zero flag
PUT1    RTS

*
*	cyclic increment of buffer pointers 
*
BUMPU   LEAX    1,X				; increment pointer
        CMPX    #BUFFER+BUFLEN	; end of buffer?
        BNE     ANRTS			; nope : exit

LFA5A   LDX     #BUFFER			; reset buffer ptr to beginning	
ANRTS   RTS

*
*	get character from buffer, if none then clears interrupt
*	mask and waits, all registers saved, including cc 
*
		SETDP	0
GETCHR  PSHS    X,CC			; save regs
        BRA     GETCH1			; skip wait			

GETCH2  CWAI    #255-IRQ		; wait for an interrupt
GETCH1  LDX     TAIL			; get tail pointer
        CMPX    HEADST			; if it equals static head
        BEQ     GETCH2			; then no characters

		BSR     BUMPU			; else move up
        LDA     ,X				; and get it
        STX     TAIL			; set new tail
        PULS    PC,X,CC         ; restore and return
	
*
*	output x register as 4 hex digits to console,
*	all registers saved.
*
		SETDP	MONDP
OPXREG  PSHS    D				; save regs
        TFR     X,D				; copy X to D
        BSR     OPAREG			; Output MSB in A

        TFR     B,A				; Transfer LSB to A
        BSR     OPARSP			; output it
        PULS    PC,D            ; restore and return

*
*	output a register as 2 hex digits to console,
*	all registers saved except a.
*
OPAREG  PSHS    A				; save A
        LSRA					; get MSN into LSN
        LSRA
        LSRA
        LSRA
        BSR     HEXOUT			; Output it

        LDA     ,S+				; restore A from stack
        ANDA    #$0F			; mask out MSN

*
*	output a as a hex digit
*
HEXOUT  ADDA    #'0				; convert to digit
        CMPA    #'9				; > 9?
        BLS     HEX2			; no : output it

        ADDA    #'A-'9-1		; adjust as a letter A..F
HEX2    BRA     CONOUT			; output it

*
*	output a as a hex digit followed by a space
*
OPARSP  BSR     OPAREG			; output digit
        LDA     #SPACE			; and a space
        BRA     HEX2

*
*	printer routine, this interfaces to anadex or centronics
*	parallel interface printers.
*
		SETDP	0
PRINT   PSHS    U,D				; save registers
        LDU     #KVIA			; point at VIA
        CMPA    PNEW			; is spec'd symbol then do not send
        BEQ     PEXIT

PWAIT1  LDB     (KORA2-KVIA),U	; check if busy?
        BMI     PWAIT1			; if so keep waiting.....

        STA     (KORA-KVIA),U	; output char
        LDB     #IKPCR-PSTRB	; strobe line low
        STB     (KPCR-KVIA),U
        LDB     #IKPCR			; then high again
        STB     (KPCR-KVIA),U		
PEXIT
        PULS    PC,U,D          ; Restore and return
		
*
*	these tables are the decision tables for the
*	memory examine and change. function.
*
MTABA 	FCB		((MTABAE-MTABA)/3)-1	; number of entries
		FCB		'V
		FDB		VADDR-MTABA		; modify break address and memory
		FCB		'G
		FDB		GADDR-MTABA		; modify go address and memory
		FCB		'P
		FDB		PADDR-MTABA		; modify proceed address and memory
		FCB		'R
		FDB		RADDR-MTABA		; modify register locations
		FCB		SPACE
		FDB		SPACEA-MTABA
		FCB		COMMA
		FDB		COMMAA-MTABA
		FCB		SEMIC
		FDB		SEMICA-MTABA
		FCB		MINUS
		FDB		MINUSA-MTABA
		FCB		1
		FDB		NOTA-MTABA
MTABAE 	EQU		*
*
MTABB 	FCB		((MTABBE-MTABB)/3)-1
		FCB		SPACE
		FDB		SPACEB-MTABB
		FCB		COMMA
		FDB		COMMAB-MTABB
		FCB		SEMIC
		FDB		SEMICB-MTABB
		FCB		MINUS
		FDB		MINUSB-MTABB
		FCB		1
		FDB		NOTB-MTABB
MTABBE EQU	*

*
*	memory examine and change routIne
*
VADDR  	LBSR    BRKOUT			; take out any break

        LDU     #CBREAK			; point to break address store
        BRA     ADDR1			

GADDR   LDU     #GSAV			; point to go address store

ADDR1   LDY     ,U				; get initial value
        BRA     DATA

PADDR   LDU     <USRSTK			; point to user PC
        LEAU    10,U
        BRA     ADDR1

RADDR   LDY     <USRSTK			; get address of cc register
RADDR1  LDU     #TEMP
        BRA     DATA


MEM	    LDU     #MSAV			; point to memory address store
        LDY     ,U				; get initial value
        CLRB					; set status zero

SPACEA  LBSR    CONCHR			; get input
		BEQ		CRA				; no address given
        LDX     #MTABA			; search address
        LBRA    DISPCH			; search table

CRD   	TSTB					; if status is -1 or 0
        BLE     CRA				; then no change
        LEAY    1,Y				; else one up

CRA     TFR     Y,X				; print out address
        LBSR    OPXREG

        LDA     ,X				; print out data
        LBSR    OPARSP

        LDB     #$01			; set status +1
        STB     <ONLINE
        BRA     DATA

NOTA    STA     <LASTC			; save for re-use
        BSR     NUMB			; get number 
        BVS     MERR			; error if none
        TFR     D,Y				; is new address

DAT1    CLRB					; set status 0

COMMAA	EQU		*
SPACEB	EQU		*
DATA    LBSR    CONCHR			; get input
        BEQ     CRD				; no data found cr

		LDX     #MTABB			; else search data table
        LBRA    DISPCH

SEMICB  TSTB					; test status
        BGE     SEMICA			; if -1 then 
        LEAY    -1,Y			; dec before
SEMICA  STY     ,U				; save new address
        RTS
		
MINUSA  equ		*
MINUSB	LEAY    -1,Y			; back down one
        TSTB					; but if status is -ve
        BGE     MIND2
        LEAY    -1,Y			; then back down 2
MIND2   BRA     DAT1			; then continue

NOTB    STA     <LASTC			; save for re-use
        BSR     NUMB			; in number
        BVS     MERR			; not number : error	

        STB     ,Y				; save it
        CMPB    ,Y				; check saved ok
        BEQ     COMMAB			; is OK

        LDX     #MQRY			; memory error message
        LBSR    STRING			; print it

COMMAB  LEAY    1,Y				; go up one
        LDB     #$FF			; with status -1		
        BRA     DATA			; then continue	

MERR    LBSR    CONCHR			; get wrong symbol
        LBRA    BADCMD			; tell user

*
*	get hex number from input stream, allow leading spaces,
*	and step on first non-hex character. return number in d.
*	with v=0, if no number then d-e, and 4...1.
*
		SETDP	0
NUMB    CLRA					; clear D
        CLRB
        PSHS    D				; save it
        BSR     GETHXS			; get first non blank as hex value
        BVS     NUMB1			; not hex

NUMB3   LDB     #$04
        LSLA					; move into high nibble
        LSLA
        LSLA
        LSLA

NUMB2   LSLA					; rotate into value
        ROL     1,S
	ifdef HIGH
		ROL		,S
	else
        ROL     0,S
	endc
        DECB
        BNE     NUMB2

        BSR     GETHEX			; get next hex digit
        BVC     NUMB3			; was hex so add it in

        ANDCC   #$FD			; clear V flag
NUMB1   PULS    PC,D            ; restore and return

*
*	gethxs - get a hex digit ignoring leading spaces
*	gethex - get • hex digit
*	both return value in a., with v.0, else set vol if non-hex
*
GETHXS  LBSR    CONCHR			; get input
        BEQ     GETH5			; on cr, no number	

        CMPA    #SPACE			; is it a space?
        BEQ     GETHXS			; then skip past

        BRA     GETH2			; otherwise change to hex

GETHEX  LBSR    CONCHR			; get input
        BEQ     GETH5			; on cr, no number

GETH2   CMPA    #'0				; validate
        BLO     GETH1			; < 0 so invalid

        CMPA    #'9
        BLS     GETH3			; char is  between '0'..'9' so valid

        CMPA    #'A				
        BLO     GETH1			; invalid

        CMPA    #'F
        BLS     GETH4			; char is 'A'..'F' so valid

GETH1   CMPA    #COMMA			; is it a comma?
        BEQ     GETH5			; yes absorb it
        STA     LASTC			; else re-use

GETH5   ORCC    #$02			; set v flag, non hex 
        RTS
		
GETH4	SUBA    #$07			; alpha offset (from numbers)
GETH3   SUBA    #'0				; number offset
        RTS

*
*	resume user program using stack as stands
*		
		SETDP	MONDP
RESUME  LEAS    2,S				; drop return address
        BSR     NUMB			; get number or zero	
        STD     <NBREAK			; and set break ignore count
RES2    BSR     BRKIN			; insert breakpoints

        PULS    PC,U,Y,X,DP,D,CC 	; restore and return

*
*	software interrupt handler, come here on breakpoint
*	either stops and displays registers or traces past
*	breakpoint and resumes.
*	
SWIHAN  LDA     #MONDP			; setup monitor DP
        TFR     A,DP
		SETDP	MONDP
		
        LDX     10,S			; backup user PC (to take out inserted SWI)
        LEAX    -1,X			
        STX     10,S
        BSR     BRKOUT			; remove breakpoint

        LDX     <NBREAK			; get count
        BEQ     RES1			; stop if zero

        LEAX    -1,X			; else decrement
        STX     <NBREAK
        BSR     TUSER1			; trace past breakpoint
        BRA     RES2			; then resume again

RES1	STS     <USRSTK
        LBSR    EXREG			; display registers
        BRA     BACK1			; stay in monitor

*
*	change number in x if one given in input stream.
*	destroys d
*
NUMBX	BSR     NUMB
        BVS     NUMBX1			; no number
        TFR     D,X
NUMBX1  RTS

*
*	user program returns here if its done
*
BACK    LEAS    -2,S			; make room for new return address
        PSHS    PC,U,Y,X,DP,D,CC 	; push all regs
		
        LDX     #BACK			; set return address
        STX     12,S
        LDB     #MONDP			; setup DP
        TFR     B,DP
        BSR     BRKOUT			; remove breakpoints
        STS     <USRSTK			; save user stack pointer

BACK1	ANDCC   #$EF			; enable irq
        LBRA    PARSE			; and resume monitor functions

*
*	go to user program, optional address specified
*	the stack pointer is reset, but the register contents
*	are maintained as listed by the • command. 
*
GOUSER  CLR     <NBREAK			; zero break count
        CLR     <NBREAK+1
        LDU     <ISTACK			; get stack pointer
        LDX     #BACK			; get return address
        STX     ,--U			; put on users stack
        LDB     #11+2			; 12 registers + return
		
GO1   	LDA     B,S				; get value and push it
        STA     ,-U				
        DECB
        BPL     GO1				; keep going till all done

        LEAS    2,U				; new stack ignores return address
        LDX     <GSAV			; saved address
        BSR     NUMBX			; change if given
        STX     <GSAV			; and restore
        STX     10,S			; put it as user PC
        BSR     BRKIN			; insert breakpoints

        PULS    PC,U,Y,X,DP,D,CC 	; jump to user program

*
*	change user breakpoint position		
*
BRKSET  BSR     BRKOUT			; ensure old breakpoint removed
        LDX     #NOBREAK		; value for no breakpoint
        BSR     NUMBX			; change if given
        STX     <CBREAK			; save it
        RTS

*
*	put breakpoint in if one exists
*
BRKIN	BSR     BRKTST			; see if breakpoint already exists
        BEQ     BRK1			; allready swi / breakpoint so done

        STA     <CINST			; else save new breakpoint
        LDA     #SWIINS			; insert SWI into code
        STA     ,X
BRK1    RTS

*
*	check break required, if not does its twice
*
BRKTST  LDX     <CBREAK			; get address
        CMPX    #NOBREAK		; No breakpoint?
        BEQ     BRK10			; no break

        LDA     ,X				; get byte at BP address
        CMPA    #SWIINS			; is it an 'SWI'? : so BP set
        RTS
		
BRK10	PULS    PC,X            ; pull saved PC and real PC (exit twice).

*
*	remove a breakpoint if one present in code
*
BRKOUT  BSR     BRKTST			; see if breakpoint exists?
        BNE     BRK2			; nope : exit

        LDA     <CINST			; get saved instruction
        STA     ,X				; replace it in code
BRK2    RTS

*
*	trace one instruction of user code
*
TUSER1	PULS    X				; get return address
        STX     <IRQRTS			; and save it
        LDA     #255-IRQ		; clear IRQ in user CC
        ANDA    ,S
        STA     ,S
        LDD     #INTDEL			; delay before interrupt
        STD     KT1CL			; set timer going
        PULS    PC,U,Y,X,DP,D,CC	; start user code going

*
*	set number of instructions to trace on each command
*
TRACEN 	LBSR    NUMB			; get 0 if no number
        STD     <NTRACE			; save result
TRACE1  RTS

*
*	trace required number of instructions then display
*	register contents and halt user
*
TRACE   LDX     <NTRACE			; get number to trace
        BEQ     TRACE1			; ignore if zero
        LEAS    2,S				; strip return address

TRACE2  STX     <CTRACE			; save number left
        BSR     TUSER1			; trace one instruction

        LDX     <CTRACE			; get number left
        LEAX    -1,X			; and decrement
        BNE     TRACE2			; repeat if more, 

        LBRA    RES1			; else halt show regs and prompt user

*
*	turn printer echo of console output on or off
*
PCNTL	LBSR    CONCHR			; get input
        BEQ     POFF			; if cr then off

        CMPA    #'+				; if plus then 
        BEQ     PON				; switch on

        STA     <LASTC			; re-use if not +
POFF    CLRA					; switch off value
PON     STA     <PFLAG			; save flag
        RTS
		
*
*	if get bad command, query it and ignore rest of line
*

BADCMD	LDX     <LINEPT			; if memory input
        LBNE    MEND			; then exit

        LDX     #CQRY			; output query string
        LBSR    STRING
        LBSR    CONOUT			; and character
        LBSR    OPCRLF			; followed by EOL

BAD1    LBSR    CONCHR			; get input
        BNE     BAD1			; if n ot CR then ignore it
        RTS

*
*	cassette file load routine, this searches for named file
*	followed by data.
*
LOAD    LBSR    NAMEIN			; get filename
        LBSR    NUMB			; get offset
        STD     <OFFSET			; save offset

LOAD2   LDX     #NAME			
LOAD4   LDA     #'0				; get name file
        BSR     GETHDR			; get header
LFCB0   BNE     LOAD4			; ignore others

        LDB     #$06			; name length
        STB     <TEMP			; save it
LOAD3   BSR     CBIN1			; get input character

        LDB     ,X+				; and name character
        CMPB    #'?				; if wildcard, then matches
        BEQ     LOAD1			

        CMPA    -1,X			; else compare characters
        BNE     LOAD2			; wrong name

LOAD1   DEC     <TEMP			; count name length
        BNE     LOAD3			; repeat for all 6

        BSR     CBIN1			; ignore checksum byte

*	
*	enter here for match without file name check
*
LOAD7   LDA     #'1				; get data
        BSR     GETHDR			; get header
        BNE     LOAD5			; wrong header

        BSR     CBIN2			; get start address
        TFR     D,X				; save while 
LFCD2
        BSR     CBIN2			; getting end address
        PSHS    D				; put end on stack
		
LOAD6	BSR     CBIN1			; get data item
        STA     ,X+				; and store it
        CMPX    ,S				; check if all bytes done
        BLS     LOAD6			; no continue loading

        BSR     CBIN1			; get checksum byte
        PULS    X				; get old end address
        LBSR    OPXREG			; show address so far

        TFR     U,D				; check summed in U
        COMB					; lower byte only
        BEQ     LOAD7			; if OK then repeat

        LDX     #LQRY			; else error message
        LBSR    STRING

        BRA     KEYON			

LOAD5	CMPA    #'9				; if not 'x9' then ignore
        BNE     LOAD7

KEYON	LDA     #CB1FLG+$80		; turn on keyboard
        STA     KIER			
        LBRA    OPCRLF			; then exit

*
*	get a header from the tape, if the expected one
*	then set a zero status, else return non—zero status
* 	initialises the checksum in u to 0
*
GETHDR  PSHS    A				; save character
        LDA     #CB1FLG			; turn off keyboard
        STA     KIER

GETHD1  BSR     CBIN1			; get byte from tape
        CMPA    #'X+$80			; if not 'X' then try again
        BNE     GETHD1

        BSR     CBIN1			; get next character
        LDU     #$0000			; setup checksum
LFD10
        CMPA    ,S+				; and compare with required
        RTS
	
*
*	get 2 bytes and form a 16 bit value in d
*	add offset since is address
*	
CBIN2	BSR     CBIN1			; get a byte from tape
        TFR     A,B				; and save while
        BSR     CBIN1			; get second byte from tape
        EXG     A,B				; swap them for correct order
        ADDD    <OFFSET			; move by offset
        RTS

*
*	get 1 byte from tape, modifying checksum to suit
*		
CBIN1   JSR     [CASINA]		; get byte then
        LEAU    A,U				; add to checksum
        RTS

*
*	software asynchronous transmitter, outputs value in a
*	as start bit, 8 data bits, 2 stop bits, at rate controlled
*	by count in delcnt
*	saves all registers
*
MCASOP  PSHS    X,D,CC			; save regs
        ORCC    #$50			; disable ints
LFD29	LDB     #11				; total length (in bits) 1+8+2 
        PSHS    B				; save len on stack
        LDB     #IKPCR-COPBIT	; low start bit
        COMA					; want data inverted

MCASO1  NOP						; get timing
        BRA     MCASO2

MCASO2  DEC     ,S				; dec bits counter
        BMI     MCASO3			; all done 

        STB     KPCR			; output bit		
        BSR     CWAIT			; wait one bit time

        ANDB    #255-COPBIT		; assume next bit is zero
        LSRA					; get next bit
        BLO     MCASO1			; it is zero output it

        ORB     #COPBIT			; it  is 1 set it
        BRA     MCASO2			; and loop again

MCASO3	LEAS    1,S				; remove counter
        PULS    PC,X,D,CC       ; restore and return

*
*	cwait waits fo- 1 bit time, destroys x
*	hwait waits 1/2 bit time, also destroys x
*
		SETDP	0
CWAIT   BSR     HWAIT			; do first half

HWAIT	LDX     DELCNT			; get required count
HW1		LEAX    -1,X			; decrement
LFD50	BNE     HW1				; keep going if nonzero
        RTS
*
*	software asynchronous receiver, gets value into a
*	saves all other registers, only gets 1 stop bit
*
; The AUTO version of this routine is over-written with some
; boot code.....
;
		SETDP	MONDP
MCASIN
	ifdef AUTO
        STX     <CBREAK
        ANDCC   #$EF			; ints on
MCASI1
        LDD     #MON			; we come back to here ?
        PSHS    D
		JMP     BOOT
		
		FDB		$E980
	else
		PSHS    X,B				
        LDA     #$80			; rotating counter
MCASI1  LDB     KIRB			; wait for 
        BMI     MCASI1			; start bit
        BSR     HWAIT			; wait 1/2 bit time
        LDB     KIRB			; recheck
	endc
        BMI     MCASI1			; start bit

MCASI2  BSR     CWAIT			; wait a whole bit time

        LDB     KIRB			; get input
        CMPX    ,S				; waste time to match loop delays
        CMPS    ,S				
        LSLB					; move bit to carry
        RORA					; then into byte
        BHS     MCASI2			; repeat for 8 bits

        BSR     CWAIT			; get into stop bit
        PULS    PC,X,B          ; restore and return
*
*	routine gets name from input stream, up to 6
*	characters long no name leaves memory unaltered.
*	any name is padded to 6 characters with spaces.
*
NAMEIN  LDX     #NAME+6			
NAM2    LBSR    CONCHR			; get a character
        BEQ     NAM1			; no name

        CMPA    #SPACE			; if space ignore
        BEQ     NAM2

        CMPA    #COMMA			; null name
        BEQ     NAM1

        LDB     #256-6			; -ve name length
NAM3    STA     B,X				; pad until 
        INCB
        BEQ     NAM6			; end of name buffer

        LBSR    CONCHR			; get next letter
        BEQ     NAM5			; on CR pad name

        CMPA    #SPACE			; on space
        BEQ     NAM4			; pad name

        CMPA    #COMMA			; on comma
        BNE     NAM3			; then use 

NAM5	LDA     #SPACE			; padding
NAM4    STA     B,X				; pad until end of name buffer
        INCB
        BNE     NAM4

NAM1	RTS

NAM6	LBSR    CONCHR			; get next input
        BEQ     NAM1			; leave if CR

        CMPA    #COMMA			; else
        BEQ     NAM1			; absorb comma
        
		STA     <LASTC			; else re-use
        RTS

*
*	save files on cassette, dumps name block, data blocks
*	as required in 256 byte blocks maximum, then end file block 1142
*	can also inhibit end of file block
*
SAVE    LDX     <CSSTRT			; modify start address 
        LBSR    NUMBX			; if required
        STX     <CSSTRT
		
        LDX     <CSEND			; modify end address if required
        LBSR    NUMBX
        STX     <CSEND

        BSR     NAMEIN			; get name

        LDB     #'0				; output name
        BSR     XHEAD			; header

        LDB     #$06			; Name length
        LEAX    -6,X			; point to name
        BSR     DATOUT			; output name
        BSR     CHKOUT			; output checksum

        LDX     <CSSTRT			; get start address
SAV6   	PSHS    X				; save start
        LDD     <CSEND			;  get end addrress
        SUBD    ,S++			; calculate length
        BLO     SAV2			; done all output

        TSTA					; if <=256 then leave alone
        BEQ     SAV5

        LDD     #$00FF			; else set to 256
		
SAV5	LEAU    D,X				; from end of block
        PSHS    U,X,D			; put start / end on stack
        LDB     #'1				; put a data
        BSR     XHEAD			; header

        LEAX    2,S				; point to start / end
        LDB     #$04			; two words
        BSR     DATOUT			; put start / end out

        PULS    U,X,D			; get all back off stack
        INCB					; modify length
        BSR     DATOUT			; and send data bytes	
        BSR     CHKOUT			; and checksum
        BRA     SAV6			; and repeat

SAV2	LBSR    CONCHR			; get input
        BEQ     SAV3			; on CR send eof block

        CMPA    #'-				; if eof inhibit
        BEQ     RTS1			; then skip x9

        STA     <LASTC			; re-use input
SAV3	LDB     #'9				; send EOF

*
*	routine to send header to block, header type in b.
*	also initialises checksum in y,
*
XHEAD	LDY     #$0000			
XH1		LEAY    -1,Y			; loop
        BNE     XH1				; delay

        LDA     #'X+$80			; send X to cassette
        BSR     CASOPI

        TFR     B,A				; get type and send
        BRA     CASOPI
*
*	data output routine, sends b data bytes starting from x
*	lb zero means 256 bytes), x moves up by b bytes
*
DATOUT	LDA     ,X+				; get data
        LEAY    A,Y				; add to checksum
        BSR     CASOPI			; send to tape

        DECB					; more bytes to send ?
        BNE     DATOUT			; yes : loop again

RTS1	RTS

*
*	send checksum to tape from y, lower byte only
*
CHKOUT	TFR     Y,D				; checksum to D
        TFR     B,A				; then get low byte
        COMA					; want result $FF
CASOPI  JMP     [CASOPA]		; send check byte

*
*	drive commands to perform a boot operation
*
*	drive parameter specification
*
DISCIT FCB    $35,$04,$0D,$14,$05,$AA	; for Shugart drive
*
*	drive bad tracks
*
		FCB		$35,$04,((DRIVE/$80)*8)+$10,$FF,$FF,$FF
*
*	mode register setup
*
		FCB		$3A+DRIVE,$02,$17,$C1
*
*	load head onto disc, starts motor 
*
		FCB		$3A+DRIVE,$02,$23,$28+DRIVE
*
*	query drive ready
*
		FCB		$2C+DRIVE,0
*
*	seek to track 0
*
		FCB		$29+DRIVE,$01,$00
*
*	query drive ready
*
		FCB		$2C+DRIVE,0
*
*	read sector 2
*
	ifdef AUTO
		FCB		$13+DRIVE,$03,$00,$01,$21
	else	
		FCB		$13+DRIVE,$03,$00,$02,$21
	endc
*
*	read starting at sector 3
*
		FCB		$13+DRIVE,$02,$00,$03
*
*	this routine bootstraps from a mini-floppy disc
*	reads sector 2 to find where to put program
*
BOOT
        LDX     #ANRTI			; set dummy 
        STX     <INMI			; interrupt routine
        LDA     #FLOPY/256		; set DP to floppy controller
        TFR     A,DP
		SETDP	FLOPY/256
		
        LDX     #DISCIT			; point to command tables
        BSR     CMDPAR			; drive parameters
        BSR     CMDPAR			; bad tracks
        BSR     CMDPAR			; mode register
        BSR     CMDPAR			; drive on
        BSR     DRVRDY			; check drive ready
        BSR     CMDPAR			; seek track 0
LFE5C   BSR     DRVRDY			; check drive ready
        BSR     CMDPAR			; read sector 2

        LDU     #BOOTADR		; put at 0
        TFR     U,Y				; and point to it
        BSR     TRNSFR			; move disk to memory

        BNE     DERR			; non zero means error

	ifdef AUTO

        LDS     STACK			; reset stack end
        JMP     BOOTADR			; jump to program
	else
        LDD     #$FF42			; Error $FF in case
        CMPD    ,Y++			; if not $FF42, 
        BNE     DERR			; error no boot present

        BSR     CMDPAR			; start read at sector 3

        LDU     ,Y++			; get address to put at
        LDA     ,Y+				; and number of sectors
        LDY     ,Y				; start of program
        ADDA    #$20			; add sector length value

BOOT1	LDB     FDCS			; get FDC status
        BITB    #$20			; if parameter register full
        BNE     BOOT1			; then wait

        STA     FDCP			; send no of sectors
        BSR     TRNSFR			; move data to memory

        BNE     DERR			; error! if nonzero

        LBSR    NUMB			; try get number
        BVC     RTS2			; if got one stay in monitor

        LDS     STACK			; reset stack end
        JMP     ,Y				; jump to program
	endc

DERR	LDX     #DQRY			; print disk errror
        LBSR    STRING

        LBSR    OPAREG			; with error number
        LBRA    OPCRLF			; and EOL

	ifdef AUTO
BOOT1
        LDY     #CRTCSV			; point to CRT initial values ROM table
        LDX     #MONDPAD		; point to start of monitor DP
        LDB     #$0B			; copy count
LFE85
        LDA     ,Y+				; get a byte from ROM
        STA     ,X+				; and put it in RAM
        DECB					; decrement counter
        BPL     LFE85			; keep going till all done

        LDA     <HDISP			; get Horizontal displayed
        DECA					; decrement
        STA     ,X+				; put in HPOS
        CLRA
LFE92
        LDB     <HDISP			; get Horizontal displayed
        LSRB					; divide by 2
        STD     ,X++			; save it	
        RTS						; return
		
		FDB		$FFFF,$FFFF,$FFFF
		FCB		$FF
	endc
	
*
*	this routine sends 1 command followed by a variable
*	number of parameters, possibly none
*	x points to command, next byte is number of parameters
*	x left pointilg after last parameter, destroys d
*
CMDPAR  LDD     ,X++			; get command and number
CP1   	TST     FDCS			; test status
        BMI     CP1				; wait if busy

        STA     FDCC			; send command
CP2		DECB					; decrement param count
        BMI     RTS2			; exit if no more

CP4		LDA     FDCS			; test parameter register full
        BITA    #$20			
        BNE     CP4				; wait if so

        LDA     ,X+				; get parameter byte
        STA     FDCP			; send to FDC
        BRA     CP2				; loop back

*
*	test if drive ready, on entry x points to read drive
*	status command sequence, on exit drive is ready and x points
*	to next command sequence
*
DRVRDY	TFR     X,U				; save pointer
DR2     TFR     U,X				; restore pointer
        BSR     CMDPAR			; ask drive for status

DR1		LDA     FDCS			; check result ready
        BITA    #$10
        BEQ     DR1				; no : keep waiting

        LDA     FDCR			; get result
        BITA    #DRIVE/128*60+4	; ready bit mask : $04
        BEQ     DR2				; not ready continue waiting
RTS2	RTS

*
*	this routine transfers data from disc to memory
*	stating at address in u. returns completion code in
*	'a' when transfer finished or error occurs
*
TRNSFR	PSHS    CC				; save CC whilst set masks	
        ORCC    #$50			; disable IRQ/FIRQ, disk is on NMI
        LDB     #$04			; data available mask
        BRA     TRN2			

TRN1	LDA     FDCD			; get data
        STA     ,U+				; store it in memory

TRN2	CWAI    #$FF			; wait for interrupt
        BITB    FDCS			; check data availble
        BNE     TRN1			; yep go get it

        PULS    CC				; get masks back
        LDA     FDCR			; then get result
        RTS

*
*	interrupt request handler, comes here on irq active
*	checks for timer 1 or keyboard interrupt, if neither
*	then complains to user.
*		
IRQHAN	LDA     #MONDP			; setup direct page
        TFR     A,DP
		SETDP	MONDP
		
        LDX     #KVIA			; point to VIA address
        LDA     (KIFR-KVIA),X	; get inerrupt flag register
        BPL     UNUSED			; not the VIA!

        ANDA    #T1IFLG			; try timer 1 flag
        BEQ     IRQH1			; not timer 1

        STA     (KIFR-KVIA),X	; clear the interrupt
        JMP     [IRQRTS]		

IRQH1	LDA     (KIFR-KVIA),X	; get inerrupt flag register
        ANDA    #CB1FLG			; try for keyboard
        BEQ     UNUSED			; query user if not

        STA     (KIFR-KVIA),X	; clear the interrupt
	ifdef AUTO
		LDA     (KIRB-KVIA),X	; read the key
	else
		LDA     <<(KIRB-KVIA),X	; read the key
    endc    
        ANDA    #$7F			; stripping unused bit
        LBSR    HAVCHR			; put into buffer

ANRTI   RTI

*
*	come here if unused interrupts are active, so
*	complain to user and stop processor
*	since cannot clear an unknown interrupt
*
UNUSED	LDX     #IERR			; query user
        LBSR    STRING			; on display
UNU1    BRA     UNU1			; stop dead.

; Setup table for s version 6845 CRTC
CRTCSV  
	ifdef VDU80
; 80 col table
        FCB    	$7E				; Horizontal total (characters)
		FCB		$50				; Horizontal displayed (characters)
		FCB		$60				; H sync position
		FCB		$A8				; H sync width	
		FCB		$1D				; Vertical total
		FCB		$02				; Vertical scan line adjust 
		FCB		$19				; V sync position
		FCB		$1A				; Interlace mode
        FCB    	$40				; Max scan line address
		FCB		$09				; Cursor start
		FCB		$68				; Cursor end
		FCB		$08				; Start address (high)
	else
; 40 col table
		FCB    	$3F				; Horizontal total (characters)
		FCB		$28				; Horizontal displayed (characters)
		FCB		$34				; H sync position
		FCB		$44				; H sync width
		FCB		$1E				; Vertical total
		FCB		$02				; Vertical scan line adjust 
		FCB		$19				; V sync position
		FCB		$1B				; Interlace mode
        FCB    	$03				; Max scan line address
		FCB		$12				; Cursor start
		FCB		$70				; Cursor end
		FCB		$13				; Start address (high)
	endc
	
; Setup table for r version 6845 CRTC
CRTCRV  FCB    	$3F				; Horizontal total (characters)
		FCB		$28				; Horizontal displayed (characters)
		FCB		$34				; H sync position
		FCB		$04				; H sync width
		FCB		$1E				; Vertical total
		FCB		$02				; Vertical scan line adjust 
		FCB		$19				; V sync position
		FCB		$1B				; Interlace mode
        FCB    	$00				; Max scan line address
		FCB		$09				; Cursor start
		FCB		$68				; Cursor end
		FCB		$09				; Start address (high)

PTAB1
        FDB    	ISTACK			; initial stack pointer
		FDB		$0000			; trace initially off

        FCB    	BSPACE			; character echoed on rubout

        FCB    	$FF				; Echo on buffer on				
		FCB		$00				; printer off
		FCB		LF				; do not send LF only CR to printer

        FDB    	205				; Initial baud rate (300 baud)
								;  110 baud use 564 ($0234)
								;  300 baud use 205 ($00CD)
								; 1200 baud use 048 ($0038)
        FDB    	DISPLA			; CON5ole output
        FDB    	GETCHR			; CON5ole input
        FDB    	MCASOP			; cassette output
        FDB    	MCASIN			; cassette input
        FDB    	PRINT			; printer routine
        FDB    	FUNCTS			; display function table
        FDB    	CMNDS			; monitor command table
        FDB    	UNUSED			; initial timer 1 routine
        FDB    	$0000			; no memory interpret
        FDB    	UNUSED			; reserved vector
        FDB    	UNUSED			; init swi3
        FDB    	UNUSED			; init swi2
        FDB    	UNUSED			; init firq
        FDB    	IRQHAN			; init IRQ
        FDB    	SWIHAN			; init swi
        FDB    	UNUSED			; init NMI
        FDB    	$0000			; init load offset	
PTAB2	equ		*				; end of table


*
*	this table contains the standard set of commands
*	provided by the monitor.
*
CMNDS 	FCB		((CMNDE-CMNDS)/3)-1 	;number of entries
		FCB		'G
		FDB		GOUSER-CMNDS		; go to program
		FCB		'M
		FDB		MEM-CMNDS			; memory examine
		FCB		'R
		FDB		EXREG-CMNDS			; examine registers
		FCB		'P
		FDB		RESUME-CMNDS		; proceed after break
		FCB		'T
		FDB		TRACEN-CMNDS		; set trace number
		FCB		'S
		FDB		SAVE-CMNDS			; save on cassette
		FCB		'L
		FDB		LOAD-CMNDS			; load from cassette
		FCB		'V
		FDB		BRKSET-CMNDS		; set break address
		FCB		'D
		FDB		BOOT-CMNDS			; disk bootstrap
		FCB		'C
		FDB		PCNTL-CMNDS			; copy to printer
		FCB		SPACE
		FDB		ANRTS-CMNDS			; ignore spaces
		FCB		COMMA
		FDB		ANRTS-CMNDS			; ignore commas
		FCB		'.
		FDB		TRACE-CMNDS			; do trace operation
		FCB		'F
		FDB		LOAD7-CMNDS			; finish file load
		FCB		1
		FDB		BADCMD-CMNDS		; default is query user
CMNDE 	EQU		*

*
*	this table contains the standard functions provided
*	by the vdu control programs,
*
	
FUNCTS 	FCB		((FUNCTE-FUNCTS)/3)-1		; number of entries
		FCB		CR
		FDB		DOCR-FUNCTS					; carriage return
		FCB		LF
		FDB		DOLF-FUNCTS					; line feed
	ifdef AUTO
		FCB		BSPACE2
		FDB		DORUB-FUNCTS				; rubout
		FCB		1
		FDB		SIMCHR-FUNCTS				; default is display it
FUNCTE 	EQU		*
		FCB		1
		FDB		SIMCHR-FUNCTS				; default is display it
	else
		FCB		BSPACE
		FDB		DORUB-FUNCTS				; rubout
		FCB		FFEED
		FDB		CRTCI-FUNCTS				; form feed
		FCB		1
		FDB		SIMCHR-FUNCTS				; default is display it
FUNCTE 	EQU		*
	endc
	
*
*	this is the list of strings used by the monitor
*
MQRY    FCC    "Rom?"

SCRLF   FCB    $0D,$0A,$00

TITLES  FCC    "CC  A  B DP    X    Y    U   PC    S"
        FCB    $0D,$0A,$00

PCMESS  FCB    $0D,$0A
        FCC    "PC]"
        FCB    $00

CQRY    FCC    "What is:"
        FCB    $00

IERR   	FCC    	"I"
LQRY	FCC		"-"
DQRY	FCC		"Err "
        FCB    	$00

*
*	this is the set of indirect jumps to redirect
*	the interrupt vector addresses. 1494
*
RESVI	JMP     [IRESV]
SWI3I	JMP     [ISWI3]
SWI2I	JMP     [ISWI2]
FIRQI	JMP     [IFIRQ]
IRQI	JMP     [IIRQ]
SWII	JMP     [ISWI]
NMII	JMP     [INMI]

	ifdef AUTO
		FCB		$FF
	else
		FCB		$00
	endc
*
*	the following hardware vectors reside In the top
*	16 bytes of memory when the monitor is in its
*	standard position.
*
		ORG		$FFF0
        FDB    RESVI
        FDB    SWI3I
        FDB    SWI2I
        FDB    FIRQI
        FDB    IRQI
        FDB    SWII
        FDB    NMII
        FDB    RESET

BeebDisEndAddr
