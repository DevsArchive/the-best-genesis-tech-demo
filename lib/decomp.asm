
; -------------------------------------------------------------------------
;
;	Mega Drive Library
;		By Ralakimus 2018
;
;	File:		decomp.asm
;	Contents:	Decompression library
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Kosinski decompression (General purpose)
; New faster version by written by vladikcomper,
; with additional improvements by MarkeyJester and Flamewing
; -------------------------------------------------------------------------
; PARAMETERS:
;	a0.l	- Source address
;	a1.l	- Destination address
; RETURNS:
;	a1.l	- End of decompressed data address
; -------------------------------------------------------------------------

_Kos_UseLUT		EQU	1
_Kos_LoopUnroll		EQU	3
_Kos_ExtremeUnrolling	EQU	1

; -------------------------------------------------------------------------

_Kos_RunBitStream macro &

	dbra	d2,.skip\@
	moveq	#7,d2				; Set repeat count to 8.
	move.b	d1,d0				; Use the remaining 8 bits.
	not.w	d3				; Have all 16 bits been used up?
	bne.s	.skip\@				; Branch if not.
	move.b	(a0)+,d0			; Get desc field low-byte.
	move.b	(a0)+,d1			; Get desc field hi-byte.
	if _Kos_UseLUT=1
		move.b	(a4,d0.w),d0		; Invert bit order...
		move.b	(a4,d1.w),d1		; ... for both bytes.
	endif
.skip\@:

	endm

; -------------------------------------------------------------------------

_Kos_ReadBit macro &

	if _Kos_UseLUT=1
		add.b	d0,d0				; Get a bit from the bitstream.
	else
		lsr.b	#1,d0				; Get a bit from the bitstream.
	endif

	endm

; -------------------------------------------------------------------------

KosDec:
	moveq	#(1<<_Kos_LoopUnroll)-1,d7
	if _Kos_UseLUT=1
		moveq	#0,d0
		moveq	#0,d1
		lea	KosDec_ByteMap(pc),a4	; Load LUT pointer.
	endif
	move.b	(a0)+,d0			; Get desc field low-byte.
	move.b	(a0)+,d1			; Get desc field hi-byte.
	if _Kos_UseLUT=1
		move.b	(a4,d0.w),d0		; Invert bit order...
		move.b	(a4,d1.w),d1		; ... for both bytes.
	endif
	moveq	#7,d2				; Set repeat count to 8.
	moveq	#0,d3				; d3 will be desc field switcher.
	bra.s	.FetchNewCode

.FetchCodeLoop:
	; Code 1 (Uncompressed byte).
	_Kos_RunBitStream
	move.b	(a0)+,(a1)+
 
.FetchNewCode:
	_Kos_ReadBit
	bcs.s	.FetchCodeLoop			; If code = 1, branch.
 
	; Codes 00 and 01.
	moveq	#-1,d5
	lea	(a1),a5
	_Kos_RunBitStream

	if _Kos_ExtremeUnrolling=1
		_Kos_ReadBit
		bcs.w	.Code_01
 
		; Code 00 (Dictionary ref. short).
		_Kos_RunBitStream
		_Kos_ReadBit
		bcs.s	.Copy45
		_Kos_RunBitStream
		_Kos_ReadBit
		bcs.s	.Copy3
		_Kos_RunBitStream
		move.b	(a0)+,d5		; d5 = displacement.
		adda.w	d5,a5
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		bra.s	.FetchNewCode

.Copy3:
		_Kos_RunBitStream
		move.b	(a0)+,d5		; d5 = displacement.
		adda.w	d5,a5
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		bra.w	.FetchNewCode

.Copy45:
		_Kos_RunBitStream
		_Kos_ReadBit
		bcs.s	.Copy5
		_Kos_RunBitStream
		move.b	(a0)+,d5		; d5 = displacement.
		adda.w	d5,a5
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		bra.w	.FetchNewCode

.Copy5:
		_Kos_RunBitStream
		move.b	(a0)+,d5		; d5 = displacement.
		adda.w	d5,a5
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		move.b	(a5)+,(a1)+
		bra.w	.FetchNewCode

	else
		moveq	#0,d4			; d4 will contain copy count.
		_Kos_ReadBit
		bcs.s	.Code_01
 
		; Code 00 (Dictionary ref. short).
		_Kos_RunBitStream
		_Kos_ReadBit
		addx.w	d4,d4
		_Kos_RunBitStream
		_Kos_ReadBit
		addx.w	d4,d4
		_Kos_RunBitStream
		move.b	(a0)+,d5		; d5 = displacement.
 
.StreamCopy:
		adda.w	d5,a5
		move.b	(a5)+,(a1)+		; Do 1 extra copy (to compensate +1 to copy counter).
 
.copy:
		move.b	(a5)+,(a1)+
		dbra	d4,.copy
		bra.w	.FetchNewCode
	endif

.Code_01:
	moveq	#0,d4				; d4 will contain copy count.
	; Code 01 (Dictionary ref. long / special).
	_Kos_RunBitStream
	move.b	(a0)+,d6			; d6 = %LLLLLLLL.
	move.b	(a0)+,d4			; d4 = %HHHHHCCC.
	move.b	d4,d5				; d5 = %11111111 HHHHHCCC.
	lsl.w	#5,d5				; d5 = %111HHHHH CCC00000.
	move.b	d6,d5				; d5 = %111HHHHH LLLLLLLL.
	if _Kos_LoopUnroll=3
		and.w	d7,d4			; d4 = %00000CCC.
	else
		andi.w	#7,d4
	endif
	bne.s	.StreamCopy			; if CCC=0, branch.
 
	; special mode (extended counter)
	move.b	(a0)+,d4			; Read cnt
	beq.s	.Quit				; If cnt=0, quit decompression.
	subq.b	#1,d4
	beq.w	.FetchNewCode			; If cnt=1, fetch a new code.
 
	adda.w	d5,a5
	move.b	(a5)+,(a1)+			; Do 1 extra copy (to compensate +1 to copy counter).
	move.w	d4,d6
	not.w	d6
	and.w	d7,d6
	add.w	d6,d6
	lsr.w	#_Kos_LoopUnroll,d4
	jmp	.LargeCopy(pc,d6.w)

.LargeCopy:
	rept (1<<_Kos_LoopUnroll)
		move.b	(a5)+,(a1)+
	endr
	dbra	d4,.LargeCopy
	bra.w	.FetchNewCode

	if _Kos_ExtremeUnrolling=1
.StreamCopy:
		adda.w	d5,a5
		move.b	(a5)+,(a1)+		; Do 1 extra copy (to compensate +1 to copy counter).
		if _Kos_LoopUnroll=3
			eor.w	d7,d4
		else
			eori.w	#7,d4
		endif
		add.w	d4,d4
		jmp	.MediumCopy(pc,d4.w)

.MediumCopy:
		rept 8
			move.b	(a5)+,(a1)+
		endr
		bra.w	.FetchNewCode
	endif

.Quit:
	rts
; -------------------------------------------------------------------------

	if _Kos_UseLUT=1
KosDec_ByteMap:
		dc.b	$00,$80,$40,$C0,$20,$A0,$60,$E0,$10,$90,$50,$D0,$30,$B0,$70,$F0
		dc.b	$08,$88,$48,$C8,$28,$A8,$68,$E8,$18,$98,$58,$D8,$38,$B8,$78,$F8
		dc.b	$04,$84,$44,$C4,$24,$A4,$64,$E4,$14,$94,$54,$D4,$34,$B4,$74,$F4
		dc.b	$0C,$8C,$4C,$CC,$2C,$AC,$6C,$EC,$1C,$9C,$5C,$DC,$3C,$BC,$7C,$FC
		dc.b	$02,$82,$42,$C2,$22,$A2,$62,$E2,$12,$92,$52,$D2,$32,$B2,$72,$F2
		dc.b	$0A,$8A,$4A,$CA,$2A,$AA,$6A,$EA,$1A,$9A,$5A,$DA,$3A,$BA,$7A,$FA
		dc.b	$06,$86,$46,$C6,$26,$A6,$66,$E6,$16,$96,$56,$D6,$36,$B6,$76,$F6
		dc.b	$0E,$8E,$4E,$CE,$2E,$AE,$6E,$EE,$1E,$9E,$5E,$DE,$3E,$BE,$7E,$FE
		dc.b	$01,$81,$41,$C1,$21,$A1,$61,$E1,$11,$91,$51,$D1,$31,$B1,$71,$F1
		dc.b	$09,$89,$49,$C9,$29,$A9,$69,$E9,$19,$99,$59,$D9,$39,$B9,$79,$F9
		dc.b	$05,$85,$45,$C5,$25,$A5,$65,$E5,$15,$95,$55,$D5,$35,$B5,$75,$F5
		dc.b	$0D,$8D,$4D,$CD,$2D,$AD,$6D,$ED,$1D,$9D,$5D,$DD,$3D,$BD,$7D,$FD
		dc.b	$03,$83,$43,$C3,$23,$A3,$63,$E3,$13,$93,$53,$D3,$33,$B3,$73,$F3
		dc.b	$0B,$8B,$4B,$CB,$2B,$AB,$6B,$EB,$1B,$9B,$5B,$DB,$3B,$BB,$7B,$FB
		dc.b	$07,$87,$47,$C7,$27,$A7,$67,$E7,$17,$97,$57,$D7,$37,$B7,$77,$F7
		dc.b	$0F,$8F,$4F,$CF,$2F,$AF,$6F,$EF,$1F,$9F,$5F,$DF,$3F,$BF,$7F,$FF
	endif

; -------------------------------------------------------------------------
; Enigma decompression (Mainly for plane mappings)
; -------------------------------------------------------------------------
; PARAMETERS:
;	d0.w	- Base tile properties (tile ID, flags, etc.)
;	a0.l	- Source address
;	a1.l	- Destination address
; RETURNS:
;	a1.l	- End of decompressed data address
; -------------------------------------------------------------------------

EniDec:
	movem.l d0-d7/a1-a5,-(sp)
	movea.w d0,a3				; Store base tile properties
	move.b	(a0)+,d0
	ext.w	d0
	movea.w d0,a5				; Store first byte, extended to word
	move.b	(a0)+,d4			; Store second byte
	lsl.b	#3,d4				; Multiply by 8
	movea.w (a0)+,a2			; Store third and fourth byte
	adda.w	a3,a2				; Add base tile properties
	movea.w (a0)+,a4			; Store fifth and sixth byte
	adda.w	a3,a4				; Add base tile properties
	move.b	(a0)+,d5			; Store seventh byte
	asl.w	#8,d5				; Shift up by a byte
	move.b	(a0)+,d5			; Store eigth byte in lower register byte
	moveq	#$10,d6				; 16 bits = 2 bytes

EniDec_Loop:
	moveq	#7,d0				; Process 7 bits at a time
	move.w	d6,d7
	sub.w	d0,d7
	move.w	d5,d1
	lsr.w	d7,d1
	andi.w	#$7F,d1				; Keep only lower 7 bits
	move.w	d1,d2
	cmpi.w	#$40,d1				; Is Bit 6 set?
	bcc.s	.getnext			; If so, branch
	moveq	#6,d0				; If not, process 6 bits instead of 7
	lsr.w	#1,d2				; Bitfield now becomes TTSSSS isntead of TTTSSSS

.getnext:
	bsr.w	EniDec_ChkGetNextByte
	andi.w	#$F,d2				; Keep only lower nibble
	lsr.w	#4,d1				; Store upper nibble (max value = 7)
	add.w	d1,d1
	jmp	EniDec_JmpTable(pc,d1.w)

EniDec_Sub0:
	move.w	a2,(a1)+			; Write to destination
	addq.w	#1,a2				; Increment
	dbf	d2,EniDec_Sub0			; Repeat
	bra.s	EniDec_Loop

EniDec_Sub4:
	move.w	a4,(a1)+			; Write to destination
	dbf	d2,EniDec_Sub4			; Repeat
	bra.s	EniDec_Loop

EniDec_Sub8:
	bsr.w	EniDec_GetInlineCopyVal

.loop1:
	move.w	d1,(a1)+
	dbf	d2,.loop1
	bra.s	EniDec_Loop

EniDec_SubA:
	bsr.w	EniDec_GetInlineCopyVal

.loop2:
	move.w	d1,(a1)+
	addq.w	#1,d1
	dbf	d2,.loop2
	bra.s	EniDec_Loop

EniDec_SubC:
	bsr.w	EniDec_GetInlineCopyVal

.loop3:
	move.w	d1,(a1)+
	subq.w	#1,d1
	dbf	d2,.loop3
	bra.s	EniDec_Loop

EniDec_SubE:
	cmpi.w	#$F,d2
	beq.s	EniDec_End

.loop4:
	bsr.w	EniDec_GetInlineCopyVal
	move.w	d1,(a1)+
	dbf	d2,.loop4
	bra.s	EniDec_Loop

; -------------------------------------------------------------------------

EniDec_JmpTable:
	bra.s	EniDec_Sub0
	bra.s	EniDec_Sub0
	bra.s	EniDec_Sub4
	bra.s	EniDec_Sub4
	bra.s	EniDec_Sub8
	bra.s	EniDec_SubA
	bra.s	EniDec_SubC
	bra.s	EniDec_SubE

; -------------------------------------------------------------------------

EniDec_End:
	subq.w	#1,a0
	cmpi.w	#16,d6				; Were we going to start on a completely new byte?
	bne.s	.norollback			; If not, branch
	subq.w	#1,a0

.norollback:
	move.w	a0,d0
	lsr.w	#1,d0				; Are we on an odd byte?
	bcc.s	.evendest			; If not, branch
	addq.w	#1,a0				; Ensure we're on an even byte

.evendest:
	movem.l	(sp)+,d0-d7/a1-a5
	rts

EniDec_GetInlineCopyVal:
	move.w	a3,d3				; Store base tile properties
	move.b	d4,d1
	add.b	d1,d1
	bcc.s	.nopriority			; If d4 was < $80
	subq.w	#1,d6				; Get next bit number
	btst	d6,d5				; Is the bit set?
	beq.s	.nopriority			; If not, branch
	ori.w	#(1<<15),d3			; Set high priority bit

.nopriority:
	add.b	d1,d1
	bcc.s	.nopal1				; If d4 < $40
	subq.w	#1,d6				; Get next bit number
	btst	d6,d5				; Is the bit set?
	beq.s	.nopal1				; If not, branch
	addi.w	#(2<<13),d3			; Set the second palette bit

.nopal1:
	add.b	d1,d1
	bcc.s	.nopal0				; If d4 was < $20
	subq.w	#1,d6				; Get next bit number
	btst	d6,d5				; Is the bit set?
	beq.s	.nopal0				; If not, branch
	addi.w	#(1<<13),d3			; Set the first palette bit

.nopal0:
	add.b	d1,d1
	bcc.s	.noyflip			; If d4 was < $10
	subq.w	#1,d6				; Get next bit number
	btst	d6,d5				; Is the bit set?
	beq.s	.noyflip			; If not, branch
	ori.w	#(1<<12),d3			; Set the Y flip bit

.noyflip:
	add.b	d1,d1
	bcc.s	.noxflip			; If d4 was < 8
	subq.w	#1,d6				; Get next bit number
	btst	d6,d5				; Is the bit set?
	beq.s	.noxflip			; If not, branch
	ori.w	#(1<<11),d3			; Set the X flip bit

.noxflip:
	move.w	d5,d1
	move.w	d6,d7				; Get remaining bits
	sub.w	a5,d7				; Subtract minimum bit number
	bcc.s	.GotEnoughBits			; If we're beyond that, branch
	move.w	d7,d6
	addi.w	#16,d6				; 16 bits = 2 bytes
	neg.w	d7				; Calculate bit deficit
	lsl.w	d7,d1				; Make space for this many bits
	move.b	(a0),d5				; Get next byte
	rol.b	d7,d5				; Make the upper X bits the lower X bits
	add.w	d7,d7
	and.w	EniDec_AndVals-2(pc,d7.w),d5	; Only keep X lower bits
	add.w	d5,d1				; Compensate for the bit deficit

.AddBits:
	move.w	a5,d0
	add.w	d0,d0
	and.w	EniDec_AndVals-2(pc,d0.w),d1	; Only keep as many bits as required
	add.w	d3,d1				; Add base tile properties
	move.b	(a0)+,d5			; Get current byte, move onto next byte
	lsl.w	#8,d5				; Shift up by a byte
	move.b	(a0)+,d5			; Store next byte in lower register byte
	rts

.GotEnoughBits:
	beq.s	.GotExactCount			; If the exact number of bits are leftover, branch
	lsr.w	d7,d1				; Remove unneeded bits
	move.w	a5,d0
	add.w	d0,d0
	and.w	EniDec_AndVals-2(pc,d0.w),d1	; Only keep as many bits as required
	add.w	d3,d1				; Add base tile properties
	move.w	a5,d0				; Store number of bits used up by inline copy
	bra.s	EniDec_ChkGetNextByte		; Move onto next byte

.GotExactCount:
	moveq	#16,d6				; 16 bits = 2 bytes
	bra.s	.AddBits

; -------------------------------------------------------------------------

EniDec_AndVals:
	dc.w	$0001, $0003, $0007, $000F
	dc.w	$001F, $003F, $007F, $00FF
	dc.w	$01FF, $03FF, $07FF, $0FFF
	dc.w	$1FFF, $3FFF, $7FFF, $FFFF

; -------------------------------------------------------------------------

EniDec_ChkGetNextByte:
	sub.w	d0,d6
	cmpi.w	#9,d6
	bcc.s	.Done
	addq.w	#8,d6				; 8 bits = 1 byte
	asl.w	#8,d5				; Shift up by a byte
	move.b	(a0)+,d5			; Store next byte in lower register byte

.Done:
	rts

; -------------------------------------------------------------------------
; Load a Kosinski Moduled queue
; -------------------------------------------------------------------------
; PARAMETERS:
;	a0.l	- Queue pointer
; -------------------------------------------------------------------------

;LoadKosMQueue:
;	move.w	(a0)+,d6			; Get number of entries
;	bmi.s	.End				; If it's negative, branch
;
;.Queue:
;	movea.l	(a0)+,a1			; Get art pointer
;	move.w	(a0)+,d2			; Get VRAM address
;	bsr.s	QueueKosM			; Queue
;	dbf	d6,.Queue			; Loop
;
;.End:
;	rts
;
;; -------------------------------------------------------------------------
;; Adds Kosinski Moduled data to the queue
;; -------------------------------------------------------------------------
;; PARAMETERS:
;;	d2.w	- Destination in VRAM
;;	a1.l	- Address of the archive
;; -------------------------------------------------------------------------
;
;QueueKosM:
;	lea	r_KosM_List.w,a2
;	tst.l	(a2)				; Is the first slot free?
;	beq.s	ProcessKosM_Init		; If it is, branch
;		
;.FindFreeSlot:
;	addq.w	#6,a2				; Otherwise, check next slot
;	tst.l	(a2)
;	bne.s	.FindFreeSlot
;	move.l	a1,(a2)+			; Store source address
;	move.w	d2,(a2)+			; Store destination VRAM address
;	rts
;
;; -------------------------------------------------------------------------
;; Initializes processing of the first module on the queue
;; -------------------------------------------------------------------------
;
;ProcessKosM_Init:
;	move.w	(a1)+,d3			; Get uncompressed size
;	cmpi.w	#$A000,d3
;	bne.s	.GotSize
;	move.w	#$8000,d3			; $A000 means $8000 for some reason
;
;.GotSize:
;	lsr.w	#1,d3
;	move.w	d3,d0
;	rol.w	#5,d0
;	andi.w	#$1F,d0				; Get number of complete modules
;	move.b	d0,r_KosM_Cnt.w
;	andi.l	#$7FF,d3			; Get size of last module in words
;	bne.s	.GotLeftover			; Branch if it's non-zero
;	subq.b	#1,r_KosM_Cnt.w		; Otherwise decrement the number of modules
;	move.l	#$800,d3			; And take the size of the last module to be $800 words
;
;.GotLeftover:
;	move.w	d3,r_KosM_Last_Sz.w
;	move.w	d2,r_KosM_Dest.w
;	move.l	a1,r_KosM_Src.w
;	addq.b	#1,r_KosM_Cnt.w		; Store total number of modules
;	rts
;
;; -------------------------------------------------------------------------
;; Processes the first module on the queue
;; -------------------------------------------------------------------------
;
;ProcessKosM:
;	tst.b	r_KosM_Cnt.w
;	beq.w	.Exit
;	bmi.s	.DecompressionStarted
;
;	cmpi.w	#(r_Kos_List_End-r_Kos_List)/8,r_Kos_Cnt.w
;	bhs.w	.Exit				; Branch if the Kosinski decompression queue is full
;	movea.l	r_KosM_List.w,a1
;	lea	r_KosM_Buf.w,a2
;	bsr.w	QueueKosData			; Add current module to decompression queue
;	ori.b	#$80,r_KosM_Cnt.w		; And set bit to signify decompression in progress
;	rts
;
;.DecompressionStarted:
;	tst.w	r_Kos_Cnt.w
;	bne.w	.Exit				; Branch if the decompression isn't complete
;
;	; Otherwise, DMA the decompressed data to VRAM
;
;	andi.b	#$7F,r_KosM_Cnt.w
;	move.l	#$1000/2,d3
;	subq.b	#1,r_KosM_Cnt.w
;	bne.s	.Skip				; Branch if it isn't the last module
;	move.w	r_KosM_Last_Sz.w,d3
;
;.Skip:
;	move.w	r_KosM_Dest.w,d2
;	move.w	d2,d0
;	add.w	d3,d0
;	add.w	d3,d0
;	move.w	d0,r_KosM_Dest.w		; Set new destination
;	move.l	r_KosM_List.w,d0
;	move.l	r_Kos_List.w,d1
;	sub.l	d1,d0
;	andi.l	#$F,d0
;	add.l	d0,d1				; Round to the nearest $10 boundary
;	move.l	d1,r_KosM_List.w		; And set new source
;	move.l	#r_KosM_Buf,d1
;	bsr.w	QueueDMA
;	
;	tst.b	r_KosM_Cnt.w
;	bne.w	.Exit				; Return if this wasn't the last module
;	
;	lea	r_KosM_List.w,a0
;	lea	(r_KosM_List+6).w,a1
;	rept (r_KosM_List_End-r_KosM_List)/6-1
;		move.l	(a1)+,(a0)+		; Otherwise, shift all entries up
;		move.w	(a1)+,(a0)+
;	endr
;	clr.l	(a0)+				; And mark the last slot as free
;	clr.w	(a0)+
;	
;	move.l	r_KosM_List.w,d0
;	beq.s	.Exit				; Return if the queue is now empty
;	movea.l	d0,a1
;	move.w	r_KosM_Dest.w,d2
;	bra.w	ProcessKosM_Init
;
;.Exit:
;	rts
;
;; -------------------------------------------------------------------------
;; Adds Kosinski-compressed data to the decompression queue
;; -------------------------------------------------------------------------
;; PARAMETERS:
;;	a1.l	- Compressed data address
;;	a2.l	- Decompression destination in RAM
;; -------------------------------------------------------------------------
;
;QueueKosData:
;	move.w	r_Kos_Cnt.w,d0
;	lsl.w	#3,d0
;	lea	r_Kos_List.w,a3
;	move.l	a1,(a3,d0.w)			; Store source
;	move.l	a2,4(a3,d0.w)			; Store destination
;	addq.w	#1,r_Kos_Cnt.w
;	rts
;
;; -------------------------------------------------------------------------
;; Checks if V-INT occured in the middle of Kosinski queue processing and
;; stores the location from which processing is to resume if it did
;; -------------------------------------------------------------------------
;
;SetKosBookmark:
;	tst.w	r_Kos_Cnt.w
;	bpl.s	.Done				; Branch if a decompression wasn't in progress
;	move.l	r_Kos_VInt.w,a0			; Get pointer to V-INT return address
;	move.l	(a0),d0				; Check address V-INT is supposed to rte to
;	cmpi.l	#ProcessKos_Main,d0
;	bcs.s	.Done
;	cmpi.l	#ProcessKos_Done,d0
;	bcc.s	.Done
;	move.l	(a0),r_Kos_Bookmark.w
;	move.l	#BackupKosRegs,(a0)		; Force V-INT to rte here instead if needed
;
;.Done:
;	rts
;
;; -------------------------------------------------------------------------
;; Processes the first entry in the Kosinski decompression queue
;; -------------------------------------------------------------------------
;
;ProcessKos:
;	tst.w	r_Kos_Cnt.w
;	beq.w	ProcessKos_Done
;	bmi.w	RestoreKosBookmark		; Branch if a decompression was interrupted by V-int
;
;ProcessKos_Main:
;	ori.w	#$8000,r_Kos_Cnt.w		; Set sign bit to signify decompression in progress
;	movea.l	r_Kos_List.w,a0
;	movea.l	r_Kos_Dest.w,a1
;	
;	; What follows is identical to the normal Kosinski decompressor
;	moveq	#(1<<_Kos_LoopUnroll)-1,d7
;	if _Kos_UseLUT=1
;		moveq	#0,d0
;		moveq	#0,d1
;		lea	KosDec_ByteMap(pc),a4	; Load LUT pointer.
;	endif
;	move.b	(a0)+,d0			; Get desc field low-byte.
;	move.b	(a0)+,d1			; Get desc field hi-byte.
;	if _Kos_UseLUT=1
;		move.b	(a4,d0.w),d0		; Invert bit order...
;		move.b	(a4,d1.w),d1		; ... for both bytes.
;	endif
;	moveq	#7,d2				; Set repeat count to 8.
;	moveq	#0,d3				; d3 will be desc field switcher.
;	bra.s	.FetchNewCode
;
;.FetchCodeLoop:
;	; Code 1 (Uncompressed byte).
;	_Kos_RunBitStream
;	move.b	(a0)+,(a1)+
;
;.FetchNewCode:
;	_Kos_ReadBit
;	bcs.s	.FetchCodeLoop			; If code = 1, branch.
;
;	; Codes 00 and 01.
;	moveq	#-1,d5
;	lea	(a1),a5
;	_Kos_RunBitStream
;	if _Kos_ExtremeUnrolling=1
;		_Kos_ReadBit
;		bcs.w	.Code_01
;
;		; Code 00 (Dictionary ref. short).
;		_Kos_RunBitStream
;		_Kos_ReadBit
;		bcs.s	.Copy45
;		_Kos_RunBitStream
;		_Kos_ReadBit
;		bcs.s	.Copy3
;		_Kos_RunBitStream
;		move.b	(a0)+,d5		; d5 = displacement.
;		adda.w	d5,a5
;		move.b	(a5)+,(a1)+
;		move.b	(a5)+,(a1)+
;		bra.s	.FetchNewCode
;
;.Copy3:
;		_Kos_RunBitStream
;		move.b	(a0)+,d5		; d5 = displacement.
;		adda.w	d5,a5
;		move.b	(a5)+,(a1)+
;		move.b	(a5)+,(a1)+
;		move.b	(a5)+,(a1)+
;		bra.w	.FetchNewCode
;
;.Copy45:
;		_Kos_RunBitStream
;		_Kos_ReadBit
;		bcs.s	.Copy5
;		_Kos_RunBitStream
;		move.b	(a0)+,d5		; d5 = displacement.
;		adda.w	d5,a5
;		move.b	(a5)+,(a1)+
;		move.b	(a5)+,(a1)+
;		move.b	(a5)+,(a1)+
;		move.b	(a5)+,(a1)+
;		bra.w	.FetchNewCode
;
;.Copy5:
;		_Kos_RunBitStream
;		move.b	(a0)+,d5		; d5 = displacement.
;		adda.w	d5,a5
;		move.b	(a5)+,(a1)+
;		move.b	(a5)+,(a1)+
;		move.b	(a5)+,(a1)+
;		move.b	(a5)+,(a1)+
;		move.b	(a5)+,(a1)+
;		bra.w	.FetchNewCode
;
;	else
;		moveq	#0,d4			; d4 will contain copy count.
;		_Kos_ReadBit
;		bcs.s	.Code_01
;
;		; Code 00 (Dictionary ref. short).
;		_Kos_RunBitStream
;		_Kos_ReadBit
;		addx.w	d4,d4
;		_Kos_RunBitStream
;		_Kos_ReadBit
;		addx.w	d4,d4
;		_Kos_RunBitStream
;		move.b	(a0)+,d5		; d5 = displacement.
;
;.StreamCopy:
;		adda.w	d5,a5
;		move.b	(a5)+,(a1)+		; Do 1 extra copy (to compensate +1 to copy counter).
;
;.copy:
;		move.b	(a5)+,(a1)+
;		dbra	d4,.copy
;		bra.w	.FetchNewCode
;	endif
;
;.Code_01:
;	moveq	#0,d4				; d4 will contain copy count.
;	; Code 01 (Dictionary ref. long / special).
;	_Kos_RunBitStream
;	move.b	(a0)+,d6			; d6 = %LLLLLLLL.
;	move.b	(a0)+,d4			; d4 = %HHHHHCCC.
;	move.b	d4,d5				; d5 = %11111111 HHHHHCCC.
;	lsl.w	#5,d5				; d5 = %111HHHHH CCC00000.
;	move.b	d6,d5				; d5 = %111HHHHH LLLLLLLL.
;	if _Kos_LoopUnroll=3
;		and.w	d7,d4			; d4 = %00000CCC.
;	else
;		andi.w	#7,d4
;	endif
;	bne.s	.StreamCopy			; if CCC=0, branch.
;
;	; special mode (extended counter)
;	move.b	(a0)+,d4			; Read cnt
;	beq.s	.Quit				; If cnt=0, quit decompression.
;	subq.b	#1,d4
;	beq.w	.FetchNewCode			; If cnt=1, fetch a new code.
;
;	adda.w	d5,a5
;	move.b	(a5)+,(a1)+			; Do 1 extra copy (to compensate +1 to copy counter).
;	move.w	d4,d6
;	not.w	d6
;	and.w	d7,d6
;	add.w	d6,d6
;	lsr.w	#_Kos_LoopUnroll,d4
;	jmp	.LargeCopy(pc,d6.w)
;
;.LargeCopy:
;	rept (1<<_Kos_LoopUnroll)
;		move.b	(a5)+,(a1)+
;	endr
;	dbra	d4,.LargeCopy
;	bra.w	.FetchNewCode
;
;	if _Kos_ExtremeUnrolling=1
;.StreamCopy:
;		adda.w	d5,a5
;		move.b	(a5)+,(a1)+		; Do 1 extra copy (to compensate +1 to copy counter).
;		if _Kos_LoopUnroll=3
;		eor.w	d7,d4
;		else
;		eori.w	#7,d4
;		endif
;		add.w	d4,d4
;		jmp	.MediumCopy(pc,d4.w)
;
;.MediumCopy:
;		rept 8
;			move.b	(a5)+,(a1)+
;		endr
;		bra.w	.FetchNewCode
;	endif
;
;.Quit:	
;	move.l	a0,r_Kos_List.w
;	move.l	a1,r_Kos_Dest.w
;	andi.w	#$7FFF,r_Kos_Cnt.w		; Clear decompression in progress bit
;	subq.w	#1,r_Kos_Cnt.w
;	beq.s	ProcessKos_Done			; Branch if there aren't any entries remaining in the queue
;	lea	r_Kos_List.w,a0
;	lea	(r_Kos_List+8).w,a1		; Otherwise, shift all entries up
;	rept (r_Kos_List_End-r_Kos_List)/8-1
;		move.l	(a1)+,(a0)+
;		move.l	(a1)+,(a0)+
;	endr
;
;ProcessKos_Done:
;	rts
;
;; -------------------------------------------------------------------------
;
;RestoreKosBookmark:
;	movem.w	r_Kos_Regs.w,d0-d6
;	movem.l	(r_Kos_Regs+(2*7)).w,a0-a1/a5
;	move.l	r_Kos_Bookmark.w,-(sp)
;	move.w	r_Kos_SR.w,-(sp)
;	moveq	#(1<<_Kos_LoopUnroll)-1,d7
;	lea	KosDec_ByteMap(pc),a4		; Load LUT poiner
;	rte
;
;; -------------------------------------------------------------------------
;
;BackupKosRegs:
;	move	sr,r_Kos_SR.w
;	movem.w	d0-d6,r_Kos_Regs.w
;	movem.l	a0-a1/a5,(r_Kos_Regs+2*7).w
;	rts
;
;; -------------------------------------------------------------------------
;; Load Kosinski compressed art into VRAM
;; -------------------------------------------------------------------------
;; PARAMETERS:
;;	a0.l	- Source address
;;	a1.l	- Destination address
;;	a2.w	- VRAM address
;; -------------------------------------------------------------------------
;
;LoadKosArt:
;	movea.l	a1,a3				; Save destination address
;	bsr.w	KosDec				; Decompress the art
;
;	suba.l	a3,a1				; Get size of decompressed art
;	move.w	a1,d3
;	lsr.w	#1,d3				; Divide by 2 for DMA
;	move.l	a3,d1				; Use destination address for DMA source
;	move.w	a2,d2				; Get destination VRAM address
;	bra.w	QueueDMA			; Queue a DMA transfer

; -------------------------------------------------------------------------
