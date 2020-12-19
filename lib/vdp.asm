
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		vdp.asm
;	Contents:	VDP library (Ultra DMA queue by Flamewing)
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Initialize the DMA queue
; -------------------------------------------------------------------------

InitDMA:
	lea	r_DMA_Queue.w,a1
	move.w	#0,(a1)				; Set stop token
	move.w	a1,r_DMA_Slot.w			; Reset DMA queue slot
	move.l	#$96959493,d1			; Base values for the DMA queue
c	= 0
	rept (r_DMA_Slot-r_DMA_Queue)/(7*2)
		movep.l	d1,2+c(a1)		; Fill DMA queue
c		= c+14
	endr
	rts

; -------------------------------------------------------------------------
; Add a DMA transfer command to the DMA queue
; -------------------------------------------------------------------------
; PARAMETERS:
;	d1.l	- Source in 68000 memory
;	d2.w	- Destination in VRAM
;	d3.w	- Transfer length in words
; -------------------------------------------------------------------------

; This option breaks DMA transfers that crosses a 128kB block into two. It is disabled by default because you can simply align the art in ROM
; and avoid the issue altogether. It is here so that you have a high-performance routine to do the job in situations where you can't align it in ROM.

Use128kbSafeDMA		EQU	1

; Option to mask interrupts while updating the DMA queue. This fixes many race conditions in the DMA funcion, but it costs 46(6/1) cycles. The
; better way to handle these race conditions would be to make unsafe callers (such as S3&K's KosM decoder) prevent these by masking off interrupts
; before calling and then restore interrupts after.

UseVIntSafeDMA		EQU	0

; Option to assume that transfer length is always less than $7FFF. Only makes sense if Use128kbSafeDMA is 1. Moreover, setting this to 1 will
; cause trouble on a 64kB DMA, so make sure you never do one if you set it to 1! Enabling this saves 4(1/0) cycles on the case where a DMA is
; broken in two and both transfers are properly queued, and nothing at all otherwise.

AssumeMax7FFFXfer	EQU	0&Use128kbSafeDMA

; Like vdpComm, but starting from an address contained in a register

vdpCommReg macro &
	reg, type, rwd, clr
	
	local	cd
cd	= v\type&v\rwd
	lsl.l	#2,\reg				; Move high bits into (word-swapped) position, accidentally moving everything else
	if ((cd)&3)<>0
		addq.w	#((cd)&3),\reg		; Add upper access type bits
	endif
	ror.w	#2,\reg				; Put upper access type bits into place, also moving all other bits into their correct
						; (word-swapped) places
	swap	\reg				; Put all bits in proper places
	if \clr<>0
		andi.w	#3,\reg			; Strip whatever junk was in upper word of reg
	endif
	if ((cd)&$FC)=$20
		tas.b	\reg			; Add in the DMA flag -- tas fails on memory, but works on registers
	elseif ((cd)&$FC)<>0
		ori.w	#(((cd)&$FC)*4),\reg	; Add in missing access type bits
	endif

	endm

; -------------------------------------------------------------------------

QueueDMA:
	if UseVIntSafeDMA=1
		move.w	sr,-(sp)		; Save current interrupt mask
		intsOff				; Mask off interrupts
	endif

	movea.w	r_DMA_Slot.w,a1
	cmpa.w	#r_DMA_Slot,a1
	beq.s	.Done				; Return if there's no more room in the queue

	lsr.l	#1,d1				; Source address is in words for the VDP registers

	if Use128kbSafeDMA=1
		move.w  d3,d0			; d0 = length of transfer in words
	
	; Compute position of last transferred word. This handles 2 cases:
	; (1) zero length DMAs transfer length actually transfer $10000 words
	; (2) (source+length)&$FFFF = 0
	
		subq.w  #1,d0
		add.w   d1,d0			; d0 = ((src_address >> 1) & $FFFF) + ((xfer_len >> 1) - 1)
		bcs.s   .DoubleTransfer		; Carry set = ($10000 << 1) = $20000, or new 128kB block
	endif

	; Store VDP commands for specifying DMA into the queue

	swap	d1				; Want the high byte first
	move.w	#$977F,d0			; Command to specify source address & $FE0000, plus bitmask for the given byte
	and.b	d1,d0				; Mask in source address & $FE0000, stripping high bit in the process
	move.w	d0,(a1)+			; Store command
	move.w	d3,d1				; Put length together with (source address & $01FFFE) >> 1...
	movep.l	d1,1(a1)			; ... and stuff them all into RAM in their proper places (movep for the win)
	lea	8(a1),a1			; Skip past all of these commands

	vdpCommReg d2,VRAM,DMA,1		; Make DMA destination command
	move.l	d2,(a1)+			; Store command

	clr.w	(a1)				; Put a stop token at the end of the used part of the queue
	move.w	a1,r_DMA_Slot.w			; Set the next free slot address, potentially undoing the above clr (this is intentional!)

.Done:
	if UseVIntSafeDMA=1
		move.w	(sp)+,sr		; Restore interrupts to previous state
	endif
	rts

	if Use128kbSafeDMA=1
.DoubleTransfer:
	
	; Hand-coded version to break the DMA transfer into two smaller transfers
	; that do not cross a 128kB boundary. This is done much faster (at the cost
	; of space) than by the method of saving parameters and calling the normal
	; DMA function twice, as Sonic3_Complete does.
	; d0 is the number of words-1 that got over the end of the 128kB boundary
	
	addq.w	#1,d0				; Make d0 the number of words past the 128kB boundary
	sub.w	d0,d3				; First transfer will use only up to the end of the 128kB boundary

	; Store VDP commands for specifying DMA into the queue
	swap	d1				; Want the high byte first

	; Sadly, all registers we can spare are in use right now, so we can't use
	; no-cost RAM source safety.
	andi.w	#$7F,d1				; Strip high bit
	ori.w	#$9700,d1			; Command to specify source address & $FE0000
	move.w	d1,(a1)+			; Store command
	addq.b	#1,d1				; Advance to next 128kB boundary (**)
	move.w	d1,12(a1)			; Store it now (safe to do in all cases, as we will overwrite later if queue got filled) (**)
	move.w	d3,d1				; Put length together with (source address & $01FFFE) >> 1...
	movep.l	d1,1(a1)			; ... and stuff them all into RAM in their proper places (movep for the win)
	lea	8(a1),a1			; Skip past all of these commands

	move.w	d2,d3				; Save for later
	vdpCommReg d2,VRAM,DMA,1		; Make DMA destination command
	move.l	d2,(a1)+			; Store command

	cmpa.w	#r_DMA_Slot,a1			; Did this command fill the queue?
	beq.s	.SkipSecondTransfer		; Branch if so

	; Store VDP commands for specifying DMA into the queue
	; The source address high byte was done above already in the comments marked
	; with (**)
	
	if AssumeMax7FFFXfer=1
		ext.l	d0			; With maximum $7FFF transfer length, bit 15 of d0 is unset here
		movep.l	d0,3(a1)		; Stuff it all into RAM at the proper places (movep for the win)
	else
		moveq	#0,d2			; Need a zero for a 128kB block start
		move.w	d0,d2			; Copy number of words on this new block...
		movep.l	d2,3(a1)		; ... and stuff it all into RAM at the proper places (movep for the win)
	endif
	lea	10(a1),a1			; Skip past all of these commands

	; d1 contains length up to the end of the 128kB boundary
	add.w	d1,d1				; Convert it into byte length...
	add.w	d3,d1				; ... and offset destination by the correct amount
	vdpCommReg d1,VRAM,DMA,1		; Make DMA destination command
	move.l	d1,(a1)+			; Store command

	clr.w	(a1)				; Put a stop token at the end of the used part of the queue
	move.w	a1,r_DMA_Slot.w			; Set the next free slot address, potentially undoing the above clr (this is intentional!)

	if UseVIntSafeDMA=1
		move.w	(sp)+,sr		; Restore interrupts to previous state
	endif
	rts

.SkipSecondTransfer:
	move.w	a1,(a1)				; Set the next free slot address, overwriting what the second (**) instruction did
	
	if UseVIntSafeDMA=1
		move.w	(sp)+,sr		; Restore interrupts to previous state
	endif
	rts
	endif

; -------------------------------------------------------------------------
; Process all the DMA commands queued
; -------------------------------------------------------------------------

ProcessDMA:
	lea	VDP_CTL,a6
	lea	r_DMA_Queue.w,a1
	move.w	a1,r_DMA_Slot.w			; Reset DMA queue slot

	rept (r_DMA_Slot-r_DMA_Queue)/(7*2)
		move.w	(a1)+,d0
		beq.w	.Done			; Branch if we reached a stop token
	
		move.w	d0,(a6)			; Issue a set of VDP commands...
		move.l	(a1)+,(a6)
		move.l	(a1)+,(a6)
		move.w	(a1)+,(a6)
		move.w	(a1)+,(a6)
	endr
	moveq	#0,d0

.Done:
	move.w	d0,r_DMA_Queue.w
	rts

; -------------------------------------------------------------------------
; Initailize sprite data
; -------------------------------------------------------------------------

InitSprites:
	moveq	#0,d0
	lea	r_Sprites.w,a0			; Sprite table buffer
	moveq	#1,d1				; Link value
	moveq	#($280/8)-1,d2			; Number of sprites

.InitLoop:
	move.l	d0,(a0)				; Move off screen
	move.l	d0,4(a0)
	move.b	d1,3(a0)			; Set link value
	addq.w	#1,d1				; Increment link value
	addq.w	#8,a0				; Next sprite
	dbf	d2,.InitLoop			; Loop
	move.b	d0,-5(a0)			; Set final link value to 0

	move.w	#79,r_Sprs_Left.w		; Reset sprites left to draw
	rts

; -------------------------------------------------------------------------
; Clear the screen
; -------------------------------------------------------------------------

InitScreen:
	lea	VDP_CTL,a0
	move.w	#$8F01,(a0)			; Set autoincrement to 1
	dmaFill	0,$A000,$6000,a0		; Clear planes, sprites, and HScroll
	move.w	#$8F02,(a0)			; Set autoincrement to 2

	clrRAM	r_HScrl,r_VScrl_End		; Clear scroll RAM
	
	bsr.s	InitSprites			; Initialize sprite data
	bra.w	InitDMA				; Initialize the DMA queue

; -------------------------------------------------------------------------
; Draw a plane map
; -------------------------------------------------------------------------
; PARAMETERS:
;	a1.l	- Pointer to plane map
;	d0.w	- VDP command
;	d1.w	- Width in tiles (minus 1)
;	d2.w	- Height in tiles (minus 1)
;	d3.w	- Base tile
;	d4.l	- VDP command delta for new rows
;		  (only if not already provided)
; -------------------------------------------------------------------------

DrawPlaneH20:
	move.l	#$400000,d4			; H20 delta
	bra.s	DrawPlane

DrawPlaneH40:
	move.l	#$800000,d4			; H40 delta
	bra.s	DrawPlane

DrawPlaneH80:
	move.l	#$1000000,d4			; H80 delta

DrawPlane:
	move.l	d0,VDP_CTL			; Set VDP command
	move.w	d1,d6				; Copy width

.RowLoop:
	move.w	(a1)+,d5			; Get tile
	add.w	d3,d5				; Add base tile
	move.w	d5,VDP_DAT			; Draw it
	dbf	d6,.RowLoop			; Loop for next tile
	add.l	d4,d0				; Next row
	dbf	d2,DrawPlane			; Loop for next row
	rts

; -------------------------------------------------------------------------
; Fill a region of a plane
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- VDP command
;	d1.w	- Width in tiles (minus 1)
;	d2.w	- Height in tiles (minus 1)
;	d3.w	- Tile to fill plane with
;	d4.l	- VDP command delta for new rows
;		  (only if not already provided)
; -------------------------------------------------------------------------

FillPlaneH20:
	move.l	#$400000,d4			; H20 delta
	bra.s	FillPlane

FillPlaneH40:
	move.l	#$800000,d4			; H40 delta
	bra.s	FillPlane

FillPlaneH80:
	move.l	#$1000000,d4			; H80 delta

FillPlane:
	move.l	d0,VDP_CTL			; Set VDP command
	move.w	d1,d5				; Copy width

.RowLoop:
	move.w	d3,VDP_DAT			; Fill plane with tile
	dbf	d5,.RowLoop			; Loop for next tile
	add.l	d4,d0				; Next row
	dbf	d2,FillPlane			; Loop for next row
	rts

; -------------------------------------------------------------------------
; Clear a region of a plane
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- VDP command
;	d1.w	- Width in tiles (minus 1)
;	d2.w	- Height in tiles (minus 1)
;	d4.l	- VDP command delta for new rows
;		  (only if not already provided)
; -------------------------------------------------------------------------

ClrPlaneH20:
	moveq	#0,d3				; Fill with 0
	bra.s	FillPlaneH20			; H20

ClrPlaneH40:
	moveq	#0,d3				; Fill with 0
	bra.s	FillPlaneH40			; H40

ClrPlaneH80:
	moveq	#0,d3				; Fill with 0
	bra.s	FillPlaneH80			; H80

; -------------------------------------------------------------------------
; Load a palette into the main palette buffer
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- Size in words (minus 1)
;	a0.l	- Palette data
; -------------------------------------------------------------------------

LoadMainPal:
	lea	r_Palette.w,a1			; Palette buffer

; -------------------------------------------------------------------------
; Load a palette into a buffer
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- Size in words (minus 1)
;	a0.l	- Palette data
;	a1.l	- Palette buffer
; -------------------------------------------------------------------------

LoadPal:
	move.w	(a0)+,(a1)+			; Copy palette data
	dbf	d0,LoadPal			; Loop
	rts

; -------------------------------------------------------------------------
; Draw the sprites from mappings
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- X position
;	d1.w	- Y position
;	d4.w	- Number of sprites to draw
;	d5.w	- Sprite tile properties
;	d6.b	- Render flags
;	a1.l	- Mappings frame data
;	a6.l	- Sprite table buffer
; -------------------------------------------------------------------------

DrawSprite:
	lsr.b	#1,d6				; Is this sprite flipped horizontally?
	bcs.s	.FlipX				; If so, branch
	lsr.b	#1,d6				; Is this sprite flipped vertically?
	bcs.w	.FlipY				; If so, branch

.Loop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	add.w	d1,d2				; Add onto Y position
	move.w	d2,(a6)+			; Store in sprite table
	move.b	(a1)+,(a6)+			; Store sprite size
	addq.w	#1,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	add.w	d0,d2				; Add onto X position
	move.w	d2,(a6)+			; Store in sprite table
	subq.w	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.Loop			; Loop if there are still enough sprites left
	rts

.FlipX:
	lsr.b	#1,d6				; Is this sprite flipped vertically?
	bcs.s	.FlipXY				; If so, branch

.FlipXLoop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	add.w	d1,d2				; Add onto Y position
	move.w	d2,(a6)+			; Store in sprite table
	move.b	(a1)+,d6			; Get sprite size
	move.b	d6,(a6)+			; Store in sprite table
	addq.w	#1,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	eori.w	#$800,d2			; Flip horizontally
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	neg.w	d2				; Negate it
	move.b	.XFlipOff(pc,d6.w),d6		; Get the X offset to apply
	sub.w	d6,d2				; Subtract the new X offset
	add.w	d0,d2				; Add onto X position
	move.w	d2,(a6)+			; Store in sprite table
	subq.w	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.FlipXLoop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------

.XFlipOff:
	dc.b	8, 8, 8, 8
	dc.b	$10, $10, $10, $10
	dc.b	$18, $18, $18, $18
	dc.b	$20, $20, $20, $20

; -------------------------------------------------------------------------

.FlipXY:
.FlipXYLoop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	neg.w	d2				; Negate it
	move.b	(a1),d6				; Get sprite sizes
	move.b	.YFlipOff(pc,d6.w),d6		; Get the Y offset to apply
	sub.w	d6,d2				; Subtract from the Y offset
	add.w	d1,d2				; Add onto Y position
	move.w	d2,(a6)+			; Store in sprite table
	move.b	(a1)+,d6			; Get sprite size
	move.b	d6,(a6)+			; Store in sprite table
	addq.w	#1,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	eori.w	#$1800,d2			; Flip horizontally and vertically
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	neg.w	d2				; Negate it
	move.b	.XFlipOff(pc,d6.w),d6		; Get the X offset to apply
	sub.w	d6,d2				; Subtract the new X offset
	add.w	d0,d2				; Add onto X position
	move.w	d2,(a6)+			; Store in sprite table
	subq.w	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.FlipXYLoop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------

.YFlipOff:
	dc.b	8, $10, $18, $20
	dc.b	8, $10, $18, $20
	dc.b	8, $10, $18, $20
	dc.b	8, $10, $18, $20

; -------------------------------------------------------------------------

.FlipY:
.FlipYLoop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	neg.w	d2				; Negate it
	move.b	(a1)+,d6			; Get sprite sizes
	move.b	d6,2(a6)			; Store in sprite table
	move.b	.YFlipOff(pc,d6.w),d6		; Get the Y offset to apply
	sub.w	d6,d2				; Subtract from the Y offset
	add.w	d1,d2				; Add onto Y position
	move.w	d2,(a6)+			; Store in sprite table
	addq.w	#2,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	eori.w	#$1000,d2			; Flip vertically
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	add.w	d0,d2				; Add onto X position
	move.w	d2,(a6)+			; Store in sprite table
	subq.w	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.FlipYLoop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------
; Draw the sprites from mappings (with boundary checks)
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- X position
;	d1.w	- Y position
;	d4.w	- Number of sprites to draw
;	d5.w	- Sprite tile properties
;	d6.b	- Render flags
;	a1.l	- Mappings frame data
;	a6.l	- Sprite table buffer
; -------------------------------------------------------------------------

DrawSpr_BndChk:
	lsr.b	#1,d6				; Is this sprite flipped horizontally?
	bcs.s	.FlipX				; If so, branch
	lsr.b	#1,d6				; Is this sprite flipped vertically?
	bcs.w	.FlipY				; If so, branch

.Loop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	add.w	d1,d2				; Add onto Y position
	cmpi.w	#-32+128,d2			; Is it above the screen?
	bls.s	.Next_YOffScr			; If so, branch
	cmpi.w	#224+128,d2			; Is it below the screen?
	bhs.s	.Next_YOffScr			; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	move.b	(a1)+,(a6)+			; Store sprite size
	addq.w	#1,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	add.w	d0,d2				; Add onto X position
	cmpi.w	#-32+128,d2			; Is it left of the screen?
	bls.s	.Next_XOffScr			; If so, branch
	cmpi.w	#320+128,d2			; Is it right of the screen?
	bhs.s	.Next_XOffScr			; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	subq.w	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.Loop			; Loop if there are still enough sprites left
	rts

.Next_XOffScr:
	subq.w	#6,a6				; Go back to the start of the current sprite entry
	dbf	d4,.Loop			; Loop if there are still enough sprites left
	rts

.Next_YOffScr:
	addq.w	#5,a1				; Go to the next sprite in the mappings in the mappings
	dbf	d4,.Loop			; Loop if there are still enough sprites left
	rts

.FlipX:
	lsr.b	#1,d6				; Is this sprite flipped vertically?
	bcs.s	.FlipXY				; If so, branch

.FlipXLoop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	add.w	d1,d2				; Add onto Y position
	cmpi.w	#-32+128,d2			; Is it above the screen?
	bls.s	.Next_YOffScrFlipX		; If so, branch
	cmpi.w	#224+128,d2			; Is it below the screen?
	bhs.s	.Next_YOffScrFlipX		; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	move.b	(a1)+,d6			; Get sprite size
	move.b	d6,(a6)+			; Store in sprite table
	addq.w	#1,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	eori.w	#$800,d2			; Flip horizontally
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	neg.w	d2				; Negate it
	move.b	.XFlips(pc,d6.w),d6		; Get the X offset to apply
	sub.w	d6,d2				; Subtract the new X offset
	add.w	d0,d2				; Add onto X position
	cmpi.w	#-32+128,d2			; Is it left of the screen?
	bls.s	.Next_XOffScrFlipX		; If so, branch
	cmpi.w	#320+128,d2			; Is it right of the screen?
	bhs.s	.Next_XOffScrFlipX		; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	subq.w	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.FlipXLoop			; Loop if there are still enough sprites left
	rts

.Next_XOffScrFlipX:
	subq.w	#6,a6				; Go back to the start of the current sprite entry
	dbf	d4,.FlipXLoop			; Loop if there are still enough sprites left
	rts

.Next_YOffScrFlipX:
	addq.w	#5,a1				; Go to the next sprite in the mappings
	dbf	d4,.FlipXLoop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------

.XFlips:
	dc.b	8, 8, 8, 8
	dc.b	$10, $10, $10, $10
	dc.b	$18, $18, $18, $18
	dc.b	$20, $20, $20, $20

; -------------------------------------------------------------------------

.FlipXY:
.FlipXYLoop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	neg.w	d2				; Negate it
	move.b	(a1),d6				; Get sprite sizes
	move.b	.YFlips(pc,d6.w),d6		; Get the Y offset to apply
	sub.w	d6,d2				; Subtract from the Y offset
	add.w	d1,d2				; Add onto Y position
	cmpi.w	#-32+128,d2			; Is it above the screen?
	bls.s	.Next_YOffScrFlipXY		; If so, branch
	cmpi.w	#224+128,d2			; Is it below the screen?
	bhs.s	.Next_YOffScrFlipXY		; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	move.b	(a1)+,d6			; Get sprite size
	move.b	d6,(a6)+			; Store in sprite table
	addq.w	#1,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	eori.w	#$1800,d2			; Flip horizontally and vertically
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	neg.w	d2				; Negate it
	move.b	.XFlips(pc,d6.w),d6		; Get the X offset to apply
	sub.w	d6,d2				; Subtract the new X offset
	add.w	d0,d2				; Add onto X position
	cmpi.w	#-32+128,d2			; Is it left of the screen?
	bls.s	.Next_XOffScrFlipXY		; If so, branch
	cmpi.w	#320+128,d2			; Is it right of the screen?
	bhs.s	.Next_XOffScrFlipXY		; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	subq.w	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.FlipXYLoop			; Loop if there are still enough sprites left
	rts

.Next_XOffScrFlipXY:
	subq.w	#6,a6				; Go back to the start of the current sprite entry
	dbf	d4,.FlipXYLoop			; Loop if there are still enough sprites left
	rts

.Next_YOffScrFlipXY:
	addq.w	#5,a1				; Go to the next sprite in the mappings
	dbf	d4,.FlipXYLoop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------

.YFlips:
	dc.b	8, $10, $18, $20
	dc.b	8, $10, $18, $20
	dc.b	8, $10, $18, $20
	dc.b	8, $10, $18, $20

; -------------------------------------------------------------------------

.FlipY:
.FlipYLoop:
	move.b	(a1)+,d2			; Get Y offset
	ext.w	d2
	neg.w	d2				; Negate it
	move.b	(a1)+,d6			; Get sprite sizes
	move.b	d6,2(a6)			; Store in sprite table
	move.b	.YFlips(pc,d6.w),d6		; Get the Y offset to apply
	sub.w	d6,d2				; Subtract from the Y offset
	add.w	d1,d2				; Add onto Y position
	cmpi.w	#-32+128,d2			; Is it above the screen?
	bls.s	.Next_YOffScrFlipY		; If so, branch
	cmpi.w	#224+128,d2			; Is it below the screen?
	bhs.s	.Next_YOffScrFlipY		; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	addq.w	#2,a6				; Skip link data
	move.w	(a1)+,d2			; Get tile properties
	add.w	d5,d2				; Add base tile properties
	eori.w	#$1000,d2			; Flip vertically
	move.w	d2,(a6)+			; Store in sprite table
	move.w	(a1)+,d2			; Get X offset
	add.w	d0,d2				; Add onto X position
	cmpi.w	#-32+128,d2			; Is it left of the screen?
	bls.s	.Next_XOffScrFlipY		; If so, branch
	cmpi.w	#320+128,d2			; Is it right of the screen?
	bhs.s	.Next_XOffScrFlipY		; If so, branch
	move.w	d2,(a6)+			; Store in sprite table
	subq.w	#1,r_Sprs_Left.w		; Decrement sprite count
	dbmi	d4,.FlipYLoop			; Loop if there are still enough sprites left
	rts

.Next_XOffScrFlipY:
	subq.w	#6,a6				; Go back to the start of the current sprite entry
	dbf	d4,.FlipYLoop			; Loop if there are still enough sprites left
	rts

.Next_YOffScrFlipY:
	addq.w	#5,a1				; Go to the next sprite in the mappings
	dbf	d4,.FlipYLoop			; Loop if there are still enough sprites left
	rts

; -------------------------------------------------------------------------
