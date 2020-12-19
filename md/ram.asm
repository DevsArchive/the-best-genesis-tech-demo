
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		ram.asm
;	Contents:	System RAM
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Define a system RAM variable
; -------------------------------------------------------------------------

sRAM macro &
	name, size

	if strcmp("\0", "")
\name\_End	EQU	__rs
		rs.b	-\size
\name		EQU	__rs
	else
		rs.\0	-\size
\name		EQU	__rs
	endif

	endm

; -------------------------------------------------------------------------

	rsreset

	sRAM.b	r_Stack,	0
	rs.b	-$100				; Stack area

	sRAM.b	r_VInt,		6		; V-INT
	sRAM.b	r_HInt,		6		; H-INT

	sRAM.b	r_VSync,	1		; VSync flag

	sRAM.b	r_Checksum,	1		; Checksum flag

	sRAM.l	r_RNG_Seed,	1		; RNG seed

	sRAM.l	r_Frame_Cnt,	1		; Frame count
	sRAM.w	r_Lag_Cnt,	1		; Lag frame count

	sRAM.b	r_Region,	1		; Hardware region

	sRAM.b	r_Ctrl_Chg,	1		; Poll controller change flag
	sRAM.b	r_P2_State,	1		; P2 controller state
	sRAM.b	r_P1_State,	1		; P2 controller state
	sRAM.b	r_Ctrl_States,	0
	sRAM.w	r_P2_Press,	1		; P2 pressed buttons
	sRAM.w	r_P2_Hold,	1		; P2 held buttons
	sRAM.b	r_P2_Ctrl,	0
	sRAM.w	r_P1_Press,	1		; P2 pressed buttons
	sRAM.w	r_P1_Hold,	1		; P2 held buttons
	sRAM.b	r_P1_Ctrl,	0
	sRAM.b	r_Ctrl,		0

	sRAM.w	r_DMA_Slot,	1		; DMA queue slot
	sRAM.b	r_DMA_Queue,	$FC		; DMA queue

	sRAM	r_Palette,	$80		; Palette buffer

	sRAM.w	r_Sprs_Left,	1		; Sprites left available
	sRAM.b	r_Sprites,	$280		; Sprite table buffer

	sRAM	r_VScrl,	$50		; VScroll buffer
	sRAM	r_HScrl,	$380		; HScroll buffer

r_VScrl_A	EQU	r_VScrl			; VScroll A
r_VScrl_B	EQU	r_VScrl+2		; VScroll B
r_HScrl_A	EQU	r_HScrl			; HScroll A
r_HScrl_B	EQU	r_HScrl+2		; HScroll B

SYS_RAM		EQU	__rs&$FFFFFF		; Beginning of system RAM

; -------------------------------------------------------------------------
; Check if user RAM goes beyond the allocated user RAM space
; -------------------------------------------------------------------------

chkRAM macro &

	if (__rs&$FFFFFF)>SYS_RAM
		inform 3,"User RAM goes $%h bytes past the end", (__rs&$FFFFFF)-SYS_RAM
	elseif (__rs&$FFFFFF)<RAM_START
		inform 3,"User RAM goes $%h bytes past the end", (RAM_END-SYS_RAM)+(__rs&$FFFFFF)
	endif

	endm

; -------------------------------------------------------------------------
