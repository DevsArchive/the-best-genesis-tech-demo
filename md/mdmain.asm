
	nolist

; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		mdmain.asm
;	Contents:	Base source
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; ASM68K build options
; -------------------------------------------------------------------------

	opt	l.				; Use "." for local labels
	opt	op+				; Optimize to PC relative addressing
	opt	os+				; Optimize short branches
	opt	ow+				; Optimize absolute long addressing
	opt	oz+				; Optimize zero displacements
	opt	oaq+				; Optimize to addq
	opt	osq+				; Optimize to subq
	opt	omq+				; Optimize to moveq
	opt	ae-				; Disable automatic evens

; -------------------------------------------------------------------------
; Includes
; -------------------------------------------------------------------------

	include	"../md/constants.asm"		; Contants
	include	"../md/macro.asm"		; Macros
	include	"../md/z80.asm"			; Z80 macro set
	include	"../md/debugger.asm"		; Debugger macro set
	include	"../md/ram.asm"			; System RAM
	list
	include	"shared.asm"			; Shared includes
	include	"config.asm"			; Configuration
	nolist

; -------------------------------------------------------------------------
; Vector table
; -------------------------------------------------------------------------

	org	0

	dc.l	r_Stack				; Stack pointer
	dc.l	ICD_BLK				; Code start

	dc.l	Exception			; Bus error
	dc.l	AddrError			; Address error
	dc.l	BadInstr			; Illegal instruction
	dc.l	DivideBy0			; Division by zero error
	dc.l	Exception			; CHK exception
	dc.l	Exception			; TRAPV exception
	dc.l	Exception			; Privilege violation
	dc.l	Exception			; TRACE exception
	dc.l	Exception			; Line A emulator
	dc.l	Exception			; Line F emulator

	dcb.l	$C, 0				; Reserved

	dc.l	Exception			; Spurious exception
	dc.l	Exception			; IRQ level 1
	dc.l	Exception			; External interrupt
	dc.l	Exception			; IRQ level 3
	dc.l	r_HInt				; Horizontal interrupt
	dc.l	Exception			; IRQ level 5
	dc.l	r_VInt				; Vertical interrupt
	dc.l	Exception			; IRQ level 7

	dc.l	Exception			; TRAP 00
	dc.l	Exception			; TRAP 01
	dc.l	Exception			; TRAP 02
	dc.l	Exception			; TRAP 03
	dc.l	Exception			; TRAP 04
	dc.l	Exception			; TRAP 05
	dc.l	Exception			; TRAP 06
	dc.l	Exception			; TRAP 07
	dc.l	Exception			; TRAP 08
	dc.l	Exception			; TRAP 09
	dc.l	Exception			; TRAP 10
	dc.l	Exception			; TRAP 11
	dc.l	Exception			; TRAP 12
	dc.l	Exception			; TRAP 13
	dc.l	Exception			; TRAP 14
	dc.l	Exception			; TRAP 15

	dcb.l	$10, 0				; Reserved

; -------------------------------------------------------------------------
; Header
; -------------------------------------------------------------------------

	dc.b	"SEGA MEGA DRIVE "		; Hardware system ID

	dc.b	"(C)T-00 "			; Company
hyr	=	(_year+1900)%10000
hmth	substr	1+((_month-1)*3), 3+((_month-1)*3), &
	"JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC"
	dcb.b	4-strlen("\#hyr"), "0"
	dc.b	"\#hyr\.\hmth"			; Year and month

hgname	substr	1, $30, "\GAME_NAME"
	dc.b	"\hgname"			; Japanese game name
	dcb.b	$30-strlen("\hgname"), " "
	dc.b	"\hgname"			; Overseas game name
	dcb.b	$30-strlen("\hgname"), " "

hday	equs	"\#_day"
	if _day<10
hday	equs	"0\#_day"
	endif
hhr	equs	"\#_hours"
	if _hours<10
hhr	equs	"0\#_hours"
	endif
hmin	equs	"\#_minutes"
	if _minutes<10
hmin	equs	"0\#_minutes"
	endif
hsec	equs	"\#_seconds"
	if _seconds<10
hsec	equs	"0\#_seconds"
	endif
	dc.b	"GM "				; Game type
	dc.b	"\hday\\hhr\\hmin\\hsec\-00"	; Game version

	dc.w	0				; Checksum

hiosup	substr	1, $10, "\IO_SUPPORT"
	dc.b	"\hiosup"			; I/O support
	dcb.b	$10-strlen("\hiosup"), " "

	dc.l	ROM_START, ROM_END-1		; ROM start and end addresses
	dc.l	RAM_START, RAM_END-1		; RAM start and end addresses

	dc.l	SRAM_SUPPORT			; SRAM support
	dc.l	SRAM_START, SRAM_END		; SRAM start and end addresses

	dc.b	"            "			; Modem information
	dc.b	"        "

hnotes	substr	1, $20, "\NOTES"		; Notes
	dc.b	"\hnotes"
	dcb.b	$20-strlen("\hnotes"), " "

	if LOCKOUT				; Region code
hregion		substr	REGIONS+1,REGIONS+1,"0123456789ABCDEF"
		dc.b	"\hregion               "
	else
		dc.b	"F               "
	endif

; -------------------------------------------------------------------------
; General exception/Restart the game
; -------------------------------------------------------------------------

Exception:
	movea.l	ROM_START.w,sp			; Reset stack pointer

; -------------------------------------------------------------------------
; Initialization
; -------------------------------------------------------------------------

ICD_BLK:
	include	"../md/icd.asm"			; Initialization
	waitDMA					; Wait for DMA to finish

	if LOCKOUT
		include	"../md/lock.asm"	; Lockout
	endif

; -------------------------------------------------------------------------

	di					; Disable interrupts
	clrRAM	RAM_START,SYS_RAM		; Clear user RAM

	move.w	#$4EF9,d0			; JMP instruction
	move.w	d0,r_HInt.w			; Set for H-INT
	move.w	d0,r_VInt.w			; Set for V-INT
	move.l	#Int_Blank,r_HInt+2.w		; Set H-INT pointer
	move.l	#VInt_Std,r_VInt+2.w		; Set V-INT pointer

	move.b	HW_VERSION,r_Region.w		; Get region

	bsr.w	LoadMegaPCM			; Load Dual PCM
	bsr.w	InitCtrls			; Initialize controllers
	bsr.w	InitScreen			; Initialize the screen

	move.w	#$8134,VDP_CTL			; Enable V-INT and DMA

	jmp	Main				; Go to main

; -------------------------------------------------------------------------
; Libraries
; -------------------------------------------------------------------------

	include	"../lib/ctrl.asm"		; Controller I/O library
	include	"../lib/vdp.asm"		; VDP library
	include	"../lib/math.asm"		; Math library
	include	"../lib/decomp.asm"		; Decompression library
	include	"../lib/int.asm"		; Interrupt library

; -------------------------------------------------------------------------
; Main source
; -------------------------------------------------------------------------

	list
	include	"main.asm"
	nolist

; -------------------------------------------------------------------------
; Error handler
; -------------------------------------------------------------------------

	include	"../md/error.asm"

; -------------------------------------------------------------------------
