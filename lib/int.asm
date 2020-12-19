
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		int.asm
;	Contents:	Interrupt library
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Perform VSync
; -------------------------------------------------------------------------

VSync:
	ei					; Enable interrupts
	tst.b	r_VSync.w			; Was the VSync flag already set?
	bne.s	.Wait				; If so, branch
	st	r_VSync.w			; Set VSync flag

.Wait:
	tst.b	r_VSync.w			; Has the V-INT run yet?
	bne.s	.Wait				; If not, wait

	rts

; -------------------------------------------------------------------------
; Standard V-INT routine
; -------------------------------------------------------------------------

VInt_Std:
	di					; Disable interrupts
	pusha					; Push all registers

	bsr.w	ReadCtrls			; Read controller data

	bsr.w	ProcessDMA			; Process DMA queue
	dma68k	r_Palette,0,$80,CRAM,a6		; Transfer palette data
	dma68k	r_HScrl,$FC00,$380,VRAM,a6	; Transfer HScroll data
	dma68k	r_VScrl,0,$50,VSRAM,a6		; Transfer VScroll data
	dma68k	r_Sprites,$F800,$280,VRAM,a6	; Transfer sprites

	ackVSync				; Acknowledge VSync
	popa					; Pop all registers

Int_Blank:
	rte

; -------------------------------------------------------------------------
