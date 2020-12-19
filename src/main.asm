
; -------------------------------------------------------------------------
;
;	Genesis Tech Demo
;		By Ralakimus 2018
;
;	File:		main.asm
;	Contents:	Main source
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; RAM
; -------------------------------------------------------------------------

	rsset	r_Local
r_Timer		rs.b	1
		rsEven

; -------------------------------------------------------------------------
; Libraries
; -------------------------------------------------------------------------

	include	"_lib/vdp.asm"			; VDP library

; -------------------------------------------------------------------------
; Load Mega PCM
; -------------------------------------------------------------------------

LoadMegaPCM:
	nop
	move.w  #$100,d0
	move.w  d0,($A11100).l
	move.w  d0,($A11200).l
	lea     (MegaPCM).l,a0
	lea     ($A00000).l,a1
	move.w  #(MegaPCM_End-MegaPCM)-1,d1

.Load:  move.b  (a0)+,(a1)+
	dbf     d1,.Load
	moveq   #0,d1
	move.w  d1,($A11200).l
	nop
	nop
	nop
	nop
	move.w  d0,($A11200).l
	move.w  d1,($A11100).l
	rts

; -------------------------------------------------------------------------
; Play a sample
; -------------------------------------------------------------------------

PlaySample:
	move.w  #$100,($A11100).l       ; stop the Z80
.0      btst    #0,($A11100).l
	bne.s   .0
	move.b  d0,$A01FFF
	move.w  #0,($A11100).l
	rts

; -------------------------------------------------------------------------
; Main
; -------------------------------------------------------------------------

Main:
	move.l	#Art_Frame1,d1
	move.w	#$20,d2
	move.w	#(Art_Frame1_End-Art_Frame1)/2,d3
	bsr.w	QueueDMA

	move.l	#Art_Frame2,d1
	move.w	#$2220,d2
	move.w	#(Art_Frame2_End-Art_Frame2)/2,d3
	bsr.w	QueueDMA

	move.l	#Art_Frame3,d1
	move.w	#$4420,d2
	move.w	#(Art_Frame3_End-Art_Frame3)/2,d3
	bsr.w	QueueDMA

	bsr.w	ProcessDMA

	lea	Map_Frame1(pc),a1
	move.l	#$60000002,d0
	moveq	#$28-1,d1
	moveq	#$1C-1,d2
	moveq	#1,d3
	bsr.w	DrawPlaneH40

	lea	Map_Frame2(pc),a1
	move.l	#$40000003,d0
	moveq	#$28-1,d1
	moveq	#$1C-1,d2
	move.w	#$2111,d3
	bsr.w	DrawPlaneH40

	lea	Map_Frame3(pc),a1
	move.l	#$60000003,d0
	moveq	#$28-1,d1
	moveq	#$1C-1,d2
	move.w	#$4221,d3
	bsr.w	DrawPlaneH40

	lea	Pal_Frames(pc),a0
	move.w	#$60>>1-1,d0
	bsr.w	LoadMainPal

	bsr.w	VSync
	move.w	#$4E73,r_VInt.w

.Reset:
	lea	Script(pc),a0
	move.w	(a0)+,d0
	bsr.w	SetFrame
	move.w	(a0)+,r_Timer.w
	btst	#6,r_Region.w
	beq.s	.PlayDAC
	move.w	r_Timer.w,d0
	mulu.w	#50,d0
	divu.w	#60,d0
	move.w	d0,r_Timer.w

.PlayDAC:
	moveq	#$FFFFFF81,d0
	bsr.w	PlaySample

	move.w	#$8174,VDP_CTL

.Loop:
	stop	#$2500
	subq.w	#1,r_Timer.w
	bne.s	.Loop

	move.w	(a0)+,d0
	bmi.s	.Reset
	bsr.s	SetFrame
	move.w	(a0)+,r_Timer.w
	btst	#6,r_Region.w
	beq.s	.Loop
	move.w	r_Timer.w,d0
	mulu.w	#50,d0
	divu.w	#60,d0
	move.w	d0,r_Timer.w
	bra.s	.Loop

; -------------------------------------------------------------------------
; Script
; -------------------------------------------------------------------------

Script:
	dc.w	0, 140
	dc.w	1, 160

	dc.w	2, 150
	dc.w	0, 120

	dc.w	2, 300
	dc.w	0, 180

	dc.w	2, 300
	dc.w	0, 440

	dc.w	2, 600
	dc.w	0, 170

	dc.w	2, 150
	dc.w	$FFFF

; -------------------------------------------------------------------------
; Set the current frame
; -------------------------------------------------------------------------

SetFrame:
	add.w	d0,d0
	add.w	d0,d0
	move.w	@Frames(pc,d0.w),VDP_CTL
	move.w	@Frames+2(pc,d0.w),VDP_CTL
	rts

@Frames:
	dc.w	$8200|($A000/$400), $8400|($A000/$2000)
	dc.w	$8200|($C000/$400), $8400|($C000/$2000)
	dc.w	$8200|($E000/$400), $8400|($E000/$2000)

; -------------------------------------------------------------------------
; Data
; -------------------------------------------------------------------------

Art_Frame1:
	incbin	"data/1.Art.bin"
Art_Frame1_End:
	even
Map_Frame1:
	incbin	"data/1.Map.bin"
	even

Art_Frame2:
	incbin	"data/2.Art.bin"
Art_Frame2_End:
	even
Map_Frame2:
	incbin	"data/2.Map.bin"
	even

Art_Frame3:
	incbin	"data/3.Art.bin"
Art_Frame3_End:
	even
Map_Frame3:
	incbin	"data/3.Map.bin"
	even
Pal_Frames:
	incbin	"data/Palette.bin"
	even

; -------------------------------------------------------------------------
; Mega PCM
; -------------------------------------------------------------------------

	include 'MegaPCM.asm'

; -------------------------------------------------------------------------
