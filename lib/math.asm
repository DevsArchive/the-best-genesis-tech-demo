
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		math.asm
;	Contents:	Math library
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Get a pseudo random number
; -------------------------------------------------------------------------
; RETURNS:
;	d0.l	- Random number
;	d1.l	- New seed
; -------------------------------------------------------------------------

RandomNumber:
	move.l	r_RNG_Seed.w,d1			; Get seed
	bne.s	.Generate			; If it's already initialized, branch
	move.l	#$2A6D365B,d1			; Initialize the seed

.Generate:
	move.l	d1,d0				; Do a bunch of calculations to get a "random" number
	asl.l	#2,d1
	add.l	d0,d1
	asl.l	#3,d1
	add.l	d0,d1
	move.w	d1,d0
	swap	d1
	add.w	d1,d0
	move.w	d0,d1
	swap	d1
	move.l	d1,r_RNG_Seed.w			; Set new seed
	rts

; -------------------------------------------------------------------------
; Calculate the sine and cosine of an angle
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.b	- Angle
; RETURNS:
;	d0.w	- Sine value
;	d1.w	- Cosine value	
; -------------------------------------------------------------------------

CalcSines:
	andi.w	#$FF,d0				; Only allow angles 0-$FF
	addq.w	#8,d0				; Turn into offset
	add.w	d0,d0
	move.w	SineTable-$10+$80(pc,d0.w),d1	; Get cosine value
	move.w	SineTable-$10(pc,d0.w),d0	; Get sine value
	rts

; -------------------------------------------------------------------------
SineTable:
	rept	2
		dc.w	$0000, $0006, $000C, $0012, $0019, $001F, $0025, $002B, $0031, $0038, $003E, $0044, $004A, $0050, $0056, $005C
		dc.w	$0061, $0067, $006D, $0073, $0078, $007E, $0083, $0088, $008E, $0093, $0098, $009D, $00A2, $00A7, $00AB, $00B0
		dc.w	$00B5, $00B9, $00BD, $00C1, $00C5, $00C9, $00CD, $00D1, $00D4, $00D8, $00DB, $00DE, $00E1, $00E4, $00E7, $00EA
		dc.w	$00EC, $00EE, $00F1, $00F3, $00F4, $00F6, $00F8, $00F9, $00FB, $00FC, $00FD, $00FE, $00FE, $00FF, $00FF, $00FF
		dc.w	$0100, $00FF, $00FF, $00FF, $00FE, $00FE, $00FD, $00FC, $00FB, $00F9, $00F8, $00F6, $00F4, $00F3, $00F1, $00EE
		dc.w	$00EC, $00EA, $00E7, $00E4, $00E1, $00DE, $00DB, $00D8, $00D4, $00D1, $00CD, $00C9, $00C5, $00C1, $00BD, $00B9
		dc.w	$00B5, $00B0, $00AB, $00A7, $00A2, $009D, $0098, $0093, $008E, $0088, $0083, $007E, $0078, $0073, $006D, $0067
		dc.w	$0061, $005C, $0056, $0050, $004A, $0044, $003E, $0038, $0031, $002B, $0025, $001F, $0019, $0012, $000C, $0006
		dc.w	$0000, $FFFA, $FFF4, $FFEE, $FFE7, $FFE1, $FFDB, $FFD5, $FFCF, $FFC8, $FFC2, $FFBC, $FFB6, $FFB0, $FFAA, $FFA4
		dc.w	$FF9F, $FF99, $FF93, $FF8B, $FF88, $FF82, $FF7D, $FF78, $FF72, $FF6D, $FF68, $FF63, $FF5E, $FF59, $FF55, $FF50
		dc.w	$FF4B, $FF47, $FF43, $FF3F, $FF3B, $FF37, $FF33, $FF2F, $FF2C, $FF28, $FF25, $FF22, $FF1F, $FF1C, $FF19, $FF16
		dc.w	$FF14, $FF12, $FF0F, $FF0D, $FF0C, $FF0A, $FF08, $FF07, $FF05, $FF04, $FF03, $FF02, $FF02, $FF01, $FF01, $FF01
		dc.w	$FF00, $FF01, $FF01, $FF01, $FF02, $FF02, $FF03, $FF04, $FF05, $FF07, $FF08, $FF0A, $FF0C, $FF0D, $FF0F, $FF12
		dc.w	$FF14, $FF16, $FF19, $FF1C, $FF1F, $FF22, $FF25, $FF28, $FF2C, $FF2F, $FF33, $FF37, $FF3B, $FF3F, $FF43, $FF47
		dc.w	$FF4B, $FF50, $FF55, $FF59, $FF5E, $FF63, $FF68, $FF6D, $FF72, $FF78, $FF7D, $FF82, $FF88, $FF8B, $FF93, $FF99
		dc.w	$FF9F, $FFA4, $FFAA, $FFB0, $FFB6, $FFBC, $FFC2, $FFC8, $FFCF, $FFD5, $FFDB, $FFE1, $FFE7, $FFEE, $FFF4, $FFFA
	endr

; -------------------------------------------------------------------------
; Calculate an angle from (0,0) to (x,y)
; -------------------------------------------------------------------------
; PARAMETERS:
;	d1.w	- X position
;	d2.w	- Y position
; -------------------------------------------------------------------------
; RETURNS:
;	d0.w	- Angle
; -------------------------------------------------------------------------

CalcAngle:
	movem.l	d3-d4,-(sp)
	moveq	#0,d3
	moveq	#0,d4
	move.w	d1,d3
	move.w	d2,d4
	or.w	d3,d4
	beq.s	.Zero				; Special case when both x and y are zero
	move.w	d2,d4

	tst.w	d3
	bpl.s	.NotNeg
	neg.w	d3

.NotNeg:
	tst.w	d4
	bpl.s	.NotNeg2
	neg.w	d4

.NotNeg2:
	cmp.w	d3,d4
	bhs.s	.NotGreater			; If |y| >= |x|
	lsl.l	#8,d4
	divu.w	d3,d4
	moveq	#0,d0
	move.b	ArcTanTable(pc,d4.w),d0
	bra.s	.Skip

.NotGreater:
	lsl.l	#8,d3
	divu.w	d4,d3
	moveq	#$40,d0
	sub.b	ArcTanTable(pc,d3.w),d0		; arctan(y/x) = 90 - arctan(x/y)

.Skip:
	tst.w	d1
	bpl.s	.Skip2
	neg.w	d0
	addi.w	#$80,d0				; Place angle in appropriate quadrant

.Skip2:
	tst.w	d2
	bpl.s	.Skip3
	neg.w	d0
	addi.w	#$100,d0			; Place angle in appropriate quadrant

.Skip3:
	movem.l	(sp)+,d3-d4
	rts

.Zero:
	move.w	#$40,d0				; Angle = 90 degrees
	movem.l	(sp)+,d3-d4
	rts

; -------------------------------------------------------------------------

ArcTanTable:
	dc.b	$00, $00, $00, $00, $01, $01
	dc.b	$01, $01, $01, $01, $02, $02
	dc.b	$02, $02, $02, $02, $03, $03
	dc.b	$03, $03, $03, $03, $03, $04
	dc.b	$04, $04, $04, $04, $04, $05
	dc.b	$05, $05, $05, $05, $05, $06
	dc.b	$06, $06, $06, $06, $06, $06
	dc.b	$07, $07, $07, $07, $07, $07
	dc.b	$08, $08, $08, $08, $08, $08
	dc.b	$08, $09, $09, $09, $09, $09
	dc.b	$09, $0A, $0A, $0A, $0A, $0A
	dc.b	$0A, $0A, $0B, $0B, $0B, $0B
	dc.b	$0B, $0B, $0B, $0C, $0C, $0C
	dc.b	$0C, $0C, $0C, $0C, $0D, $0D
	dc.b	$0D, $0D, $0D, $0D, $0D, $0E
	dc.b	$0E, $0E, $0E, $0E, $0E, $0E
	dc.b	$0F, $0F, $0F, $0F, $0F, $0F
	dc.b	$0F, $10, $10, $10, $10, $10
	dc.b	$10, $10, $11, $11, $11, $11
	dc.b	$11, $11, $11, $11, $12, $12
	dc.b	$12, $12, $12, $12, $12, $13
	dc.b	$13, $13, $13, $13, $13, $13
	dc.b	$13, $14, $14, $14, $14, $14
	dc.b	$14, $14, $14, $15, $15, $15
	dc.b	$15, $15, $15, $15, $15, $15
	dc.b	$16, $16, $16, $16, $16, $16
	dc.b	$16, $16, $17, $17, $17, $17
	dc.b	$17, $17, $17, $17, $17, $18
	dc.b	$18, $18, $18, $18, $18, $18
	dc.b	$18, $18, $19, $19, $19, $19
	dc.b	$19, $19, $19, $19, $19, $19
	dc.b	$1A, $1A, $1A, $1A, $1A, $1A
	dc.b	$1A, $1A, $1A, $1B, $1B, $1B
	dc.b	$1B, $1B, $1B, $1B, $1B, $1B
	dc.b	$1B, $1C, $1C, $1C, $1C, $1C
	dc.b	$1C, $1C, $1C, $1C, $1C, $1C
	dc.b	$1D, $1D, $1D, $1D, $1D, $1D
	dc.b	$1D, $1D, $1D, $1D, $1D, $1E
	dc.b	$1E, $1E, $1E, $1E, $1E, $1E
	dc.b	$1E, $1E, $1E, $1E, $1F, $1F
	dc.b	$1F, $1F, $1F, $1F, $1F, $1F
	dc.b	$1F, $1F, $1F, $1F, $20, $20
	dc.b	$20, $20, $20, $20, $20, $00

; -------------------------------------------------------------------------
