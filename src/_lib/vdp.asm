
; -------------------------------------------------------------------------
;
;	Genesis Tech Demo
;		By Ralakimus 2018
;
;	File:		vdp.asm
;	Contents:	VDP library
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Load a palette into the fade palette buffer
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- Size in words (minus 1)
;	a0.l	- Palette data
; -------------------------------------------------------------------------

LoadFadePal:
	lea	r_Fade_Pal.w,a1			; Fade palette buffer
	bra.w	LoadPal				; Load it

; -------------------------------------------------------------------------
; Fade the palette to black
; -------------------------------------------------------------------------

FadeToBlack:
	move.w	#$003F,r_Fade.w			; Set to fade everything

FadeToBlack_Range:
	moveq	#7,d4				; Set repeat times
	
.FadeLoop:
	bsr.w	VSync				; Do V-SYNC
	bsr.w	VSync
	bsr.s	FadeToBlack_Once		; Fade the colors once
	dbf	d4,.FadeLoop			; Loop until we are done
	rts

; -------------------------------------------------------------------------

FadeToBlack_Once:
	moveq	#0,d0
	lea	r_Palette.w,a0			; Palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoop:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoop			; Loop

	moveq	#0,d0
	lea	r_Water_Pal.w,a0		; Water palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoopWater:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoopWater		; Loop
	rts

; -------------------------------------------------------------------------

.FadeColor:
	move.w	(a0),d5				; Load color
	beq.s	.NoRed				; If the color is already black, branch
	move.w	d5,d1				; Copy color
	move.b	d1,d2				; Load green and red
	move.b	d1,d3				; Load only red

	andi.w	#$E00,d1			; Get only blue
	beq.s	.NoBlue				; If blue is finished, branch
	subi.w	#$200,d5			; Decrease blue

.NoBlue:
	andi.b	#$E0,d2				; Get only green
	beq.s	.NoGreen			; If green is finished, branch
	subi.w	#$20,d5				; Decrease green

.NoGreen:
	andi.b	#$E,d3				; Get only red
	beq.s	.NoRed				; If red is finished, branch
	subq.w	#2,d5				; Decrease red

.NoRed:
	move.w	d5,(a0)+			; Save the color
	rts

; -------------------------------------------------------------------------
; Fade the palette from black to the target palette
; -------------------------------------------------------------------------

FadeFromBlack:
	move.w	#$003F,r_Fade.w			; Set to fade everything

FadeFromBlack_Range:
	moveq	#$E,d4				; Maximum color check

.FadeLoop:
	bsr.w	VSync				; Do V-SYNC
	bsr.w	VSync
	bsr.s	FadeFromBlack_Once		; Fade the colors once
	subq.b	#2,d4				; Decrement color check
	bne.s	.FadeLoop			; If we are not done, branch

	bra.w	VSync				; Do V-SYNC so that the colors transfer

; -------------------------------------------------------------------------

FadeFromBlack_Once:
	moveq	#0,d0
	lea	r_Palette.w,a0			; Palette buffer
	lea	r_Fade_Pal.w,a1			; Target palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	adda.w	d0,a1
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoop:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoop			; Loop

	moveq	#0,d0
	lea	r_Water_Pal.w,a0		; Water palette buffer
	lea	r_UW_Fade_Pal.w,a1		; Target water palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	adda.w	d0,a1
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoopWater:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoopWater		; Loop
	rts

; -------------------------------------------------------------------------

.FadeColor:
	move.b	(a1),d5				; Load blue
	move.w	(a1)+,d1			; Load green and red
	move.b	d1,d2				; Load red
	lsr.b	#4,d1				; Get only green
	andi.b	#$E,d2				; Get only red

	move.w	(a0),d3				; Load current color
	cmp.b	d5,d4				; Should the blue fade?
	bhi.s	.NoBlue				; If not, branch
	addi.w	#$200,d3			; Increase blue

.NoBlue:
	cmp.b	d1,d4				; Should the green fade?
	bhi.s	.NoGreen			; If not, branch
	addi.w	#$20,d3				; Increase green

.NoGreen:
	cmp.b	d2,d4				; Should the red fade?
	bhi.s	.NoRed				; If not, branch
	addq.w	#2,d3				; Increase red

.NoRed:
	move.w	d3,(a0)+			; Save the color
	rts

; -------------------------------------------------------------------------
; Fade the palette to white
; -------------------------------------------------------------------------

FadeToWhite:
	move.w	#$003F,r_Fade.w			; Set to fade everything

FadeToWhite_Range:
	moveq	#7,d4				; Set repeat times

.FadeLoop:
	bsr.w	VSync				; Do V-SYNC
	bsr.w	VSync
	bsr.s	FadeToWhite_Once		; Fade the colors once
	dbf	d4,.FadeLoop			; Loop until we are done
	rts

; -------------------------------------------------------------------------

FadeToWhite_Once:
	moveq	#0,d0
	lea	r_Palette.w,a0			; Palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoop:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoop			; Loop

	moveq	#0,d0
	lea	r_Water_Pal.w,a0		; Water palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoopWater:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoopWater		; Loop
	rts

; -------------------------------------------------------------------------

.FadeColor:
	move.w	(a0),d5				; Load color
	cmpi.w	#$EEE,d5			; Is it already white?
	beq.s	.NoRed				; If so, branch
	move.w	d5,d1				; Copy color
	move.b	d1,d2				; Load green and red
	move.b	d1,d3				; Load only red

	andi.w	#$E00,d1			; Get only blue
	cmpi.w	#$E00,d1			; Is blue finished?
	beq.s	.NoBlue				; If do, branch
	addi.w	#$200,d5			; Increase blue

.NoBlue:
	andi.b	#$E0,d2				; Get only green
	cmpi.b	#$E0,d2				; Is green finished?
	beq.s	.NoGreen			; If so, branch
	addi.w	#$20,d5				; Increase green

.NoGreen:
	andi.b	#$E,d3				; Get only red
	cmpi.b	#$E,d3				; Is red finished?
	beq.s	.NoRed				; If so, branch
	addq.w	#2,d5				; Increase red

.NoRed:
	move.w	d5,(a0)+			; Save the color
	rts

; -------------------------------------------------------------------------
; Fade the palette from white to the target palette
; -------------------------------------------------------------------------

FadeFromWhite:
	move.w	#$003F,r_Fade.w			; Set to fade everything

FadeFromWhite_Range:
	moveq	#0,d4				; Minimum color check
	
.FadeLoop:
	bsr.w	VSync				; Do V-SYNC
	bsr.w	VSync
	bsr.s	FadeFromWhite_Once		; Fade the colors once
	addq.b	#2,d4				; Decrement color check
	cmpi.b	#$E,d4				; Are we done?
	bne.s	.FadeLoop			; If not, branch

	bra.w	VSync				; Do V-SYNC so that the colors transfer

; -------------------------------------------------------------------------

FadeFromWhite_Once:
	moveq	#0,d0
	lea	r_Palette.w,a0			; Palette buffer
	lea	r_Fade_Pal.w,a1			; Target palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	adda.w	d0,a1
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoop:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoop			; Loop

	moveq	#0,d0
	lea	r_Water_Pal.w,a0		; Water palette buffer
	lea	r_UW_Fade_Pal.w,a1		; Target water palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	adda.w	d0,a1
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoopWater:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoopWater		; Loop
	rts

; -------------------------------------------------------------------------

.FadeColor:
	move.b	(a1),d5				; Load blue
	move.w	(a1)+,d1			; Load green and red
	move.b	d1,d2				; Load red
	lsr.b	#4,d1				; Get only green
	andi.b	#$E,d2				; Get only red

	move.w	(a0),d3				; Load current color
	cmp.b	d5,d4				; Should the blue fade?
	bcs.s	.NoBlue				; If not, branch
	subi.w	#$200,d3			; Decrease blue

.NoBlue:
	cmp.b	d1,d4				; Should the green fade?
	bcs.s	.NoGreen			; If not, branch
	subi.w	#$20,d3				; Decrease green

.NoGreen:
	cmp.b	d2,d4				; Should the red fade?
	bcs.s	.NoRed				; If not, branch
	subq.w	#2,d3				; Decrease red

.NoRed:
	move.w	d3,(a0)+			; Save the color
	rts

; -------------------------------------------------------------------------
; Fade the palette from the current palette to the target palette
; -------------------------------------------------------------------------

FadeToPal:
	move.w	#$003F,r_Fade.w			; Set to fade everything

FadeToPal_Range:
	moveq	#0,d0
	lea	r_Palette.w,a0			; Palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0

	moveq	#7,d4				; Set repeat times

.FadeLoop:
	bsr.w	VSync				; Do V-SYNC
	bsr.w	VSync
	bsr.s	FadeToPal_Once			; Fade the colors once
	dbf	d4,.FadeLoop			; Loop until we are done
	rts

; -------------------------------------------------------------------------

FadeToPal_Once:
	moveq	#0,d0
	lea	r_Palette.w,a0			; Palette buffer
	lea	r_Fade_Pal.w,a1			; Target palette buffer
	move.b	r_Fade_Start.w,d0		; Add starting index offset
	adda.w	d0,a0
	adda.w	d0,a1
	move.b	r_Fade_Len.w,d0			; Get fade size

.FadeLoop:
	bsr.s	.FadeColor			; Fade a color			
	dbf	d0,.FadeLoop			; Loop

	rts

; -------------------------------------------------------------------------

.FadeColor:
	move.w	(a0),d3				; Get color
	cmp.w	(a1)+,d3			; Has the color already reached the target color?
	beq.s	.NoRed				; If so, branch
	
	move.w	-2(a1),d1			; Get green and red
	move.b	d1,d2				; Get red only
	andi.b	#$E,d2
	lsr.b	#4,d1				; Get green only

	move.b	-2(a1),d5			; Get blue
	cmp.b	(a0),d5				; Does blue need to fade?
	beq.s	.NoBlue				; If not, branch
	bcs.s	.DecBlue			; If it needs to be decreased, branch
	addi.w	#$200,d3			; Increase blue
	bra.s	.NoBlue				; Continue

.DecBlue:
	subi.w	#$200,d3			; Decrease blue

.NoBlue:
	move.w	(a0),d5				; Get green
	lsr.b	#4,d5
	cmp.b	d5,d1				; Does green need to fade?
	beq.s	.NoGreen			; If not, branch
	bcs.s	.DecGreen			; If it needs to be decreased, branch
	addi.b	#$20,d3				; Increase green
	bra.s	.NoGreen			; Continue

.DecGreen:
	subi.b	#$20,d3				; Decrease green

.NoGreen:
	move.w	(a0),d5				; Get red
	andi.b	#$E,d5
	cmp.b	d5,d2				; Does red need to fade?
	beq.s	.NoRed				; If not, branch
	bcs.s	.DecRed				; If it needs to be decreased, branch
	addq.b	#2,d3				; Increase red
	bra.s	.NoRed				; Continue

.DecRed:
	subq.b	#2,d3				; Decrease red

.NoRed:
	move.w	d3,(a0)+			; Save new color
	rts

; -------------------------------------------------------------------------
