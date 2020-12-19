
; -------------------------------------------------------------------------
;
;	Genesis Tech Demo
;		By Ralakimus 2018
;
;	File:		ram.asm
;	Contents:	User RAM definitions
;
; -------------------------------------------------------------------------

	; Buffers

	rsset RAM_START

r_Buffer	rs.b	$8000			; General buffer

	rsset (RAM_START+$8000)|$FF000000

r_Fade_Pal	rs.b	$80			; Fade palette buffer
r_Water_Pal	rs.b	$80			; Water palette buffer
r_UW_Fade_Pal	rs.b	$80			; Fade water palette buffer

	; Global variables

r_Fade		rs.b	0			; Fade properties
r_Fade_Start	rs.b	1			; Fade starting index
r_Fade_Len	rs.b	1			; Fade length

	; Local variables

r_Local		rs.b	0
		rs.b	SYS_RAM-(r_Local&$FFFFFF)
r_Local_End	rs.b	0

	chkRAM

; -------------------------------------------------------------------------
; Check if too much local RAM is allocated
; -------------------------------------------------------------------------
; PARAMETERS:
;	name	- Name of local variable space
; -------------------------------------------------------------------------

	nolist
chkLocal macro &
	name

	local name2
name2	equs	\name
	if (__rs&$FFFFFF)>(r_Local_End&$FFFFFF)
		inform 3,"\name2 RAM goes $%h bytes past the end", (__rs&$FFFFFF)-(r_Local_End&$FFFFFF)
	elseif (__rs&$FFFFFF)<(r_Local&$FFFFFF)
		inform 3,"\name2 RAM goes $%h bytes past the end", (RAM_END-(r_Local_End&$FFFFFF))+(__rs&$FFFFFF)
	endif

	endm
	list

; -------------------------------------------------------------------------
