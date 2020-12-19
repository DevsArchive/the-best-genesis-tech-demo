
; -------------------------------------------------------------------------
;
;	Mega Drive Base
;		By Novedicus 2018
;
;	File:		const.asm
;	Contents:	Constant definitions
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; ROM
; -------------------------------------------------------------------------

ROM_START	EQU	$000000			; ROM start
ROM_END		EQU	$400000			; ROM end

; -------------------------------------------------------------------------
; Sound
; -------------------------------------------------------------------------

YM_ADR_0	EQU	$A04000			; YM address port 0
YM_DAT_0	EQU	$A04001			; YM data port 0
YM_ADR_1	EQU	$A04002			; YM address port 1
YM_DAT_1	EQU	$A04002			; YM data port 1
PSG_CTL		EQU	$C00011			; PSG control port

; -------------------------------------------------------------------------
; I/O
; -------------------------------------------------------------------------

HW_VERSION	EQU	$A10001			; Hardware version
IO_A_DAT	EQU	$A10003			; I/O port A data port
IO_B_DAT	EQU	$A10005			; I/O port B data port
IO_C_DAT	EQU	$A10007			; I/O port C data port
IO_A_CTL	EQU	$A10009			; I/O port A control port
IO_B_CTL	EQU	$A1000B			; I/O port B control port
IO_C_CTL	EQU	$A1000D			; I/O port C control port
CART_MODE	EQU	$A11000			; Cart mode (D-RAM/ROM)
SRAM_ENABLE	EQU	$A130F1			; SRAM enable port
TMSS_SEGA	EQU	$A14000			; TMSS "SEGA" register
TMSS_MODE	EQU	$A14100			; TMSS bus mode

; -------------------------------------------------------------------------
; Z80
; -------------------------------------------------------------------------

Z80_RAM		EQU	$A00000			; Z80 RAM start
Z80_END		EQU	$A02000			; Z80 RAM end
Z80_BUS		EQU	$A11100			; Z80 bus request
Z80_RESET	EQU	$A11200			; Z80 reset

; -------------------------------------------------------------------------
; VDP
; -------------------------------------------------------------------------

VDP_DAT		EQU	$C00000			; VDP data port
VDP_CTL		EQU	$C00004			; VDP control port
VDP_HV		EQU	$C00008			; VDP H/V counter
VDP_DEBUG	EQU	$C0001C			; VDP debug

; -------------------------------------------------------------------------
; RAM
; -------------------------------------------------------------------------

RAM_START	EQU	$FF0000			; RAM start
RAM_END		EQU	$1000000		; RAM end

; -------------------------------------------------------------------------
; CD
; -------------------------------------------------------------------------

CD_BASE		EQU	$400000			; Base CD address in mode 1
;CD_BASE	EQU	$000000			; Base CD address in mode 2

CD_OS		EQU	CD_BASE+$000000		; CD OS ROM
CD_BIOS		EQU	CD_BASE+$010000		; CD BIOS system
PRG_RAM		EQU	CD_BASE+$020000		; PRG RAM bank
WORD_RAM	EQU	CD_BASE+$200000		; Word RAM
V_IMG_256	EQU	CD_BASE+$220000		; VRAM image 32 cell
V_IMG_128	EQU	CD_BASE+$230000		; VRAM image 16 cell
V_IMG_64	EQU	CD_BASE+$238000		; VRAM image 8 cell
V_IMG_32_0	EQU	CD_BASE+$23C000		; VRAM image 4 cell 0
V_IMG_32_1	EQU	CD_BASE+$23E000		; VRAM image 4 cell 1

CD_INT		EQU	$A12000			; Interrupt
CD_RESET	EQU	$A12001			; Reset
CD_PROTECT	EQU	$A12002			; Write protect
CD_MEM_MODE	EQU	$A12003			; Memory mode
CDC_MODE	EQU	$A12004			; CDC mode
CD_HINT		EQU	$A12006			; H-INT vector
CDC_HOST	EQU	$A12008			; CDC data port
CD_CMD_0	EQU	$A12010			; Communication command 0
CD_CMD_2	EQU	$A12012			; Communication command 2
CD_CMD_4	EQU	$A12014			; Communication command 4
CD_CMD_6	EQU	$A12016			; Communication command 6
CD_CMD_8	EQU	$A12018			; Communication command 8
CD_CMD_10	EQU	$A1201A			; Communication command 10
CD_CMD_12	EQU	$A1201C			; Communication command 12
CD_CMD_14	EQU	$A1201E			; Communication command 14
CD_STAT_0	EQU	$A12010			; Communication status 0
CD_STAT_2	EQU	$A12012			; Communication status 2
CD_STAT_4	EQU	$A12014			; Communication status 4
CD_STAT_6	EQU	$A12016			; Communication status 6
CD_STAT_8	EQU	$A12018			; Communication status 8
CD_STAT_10	EQU	$A1201A			; Communication status 10
CD_STAT_12	EQU	$A1201C			; Communication status 12
CD_STAT_14	EQU	$A1201E			; Communication status 14

; -------------------------------------------------------------------------
; Controller I/O
; -------------------------------------------------------------------------

	rsreset
JbU			rs.b	1		; Bit up
JbD			rs.b	1		; Bit down
JbL			rs.b	1		; Bit left
JbR			rs.b	1		; Bit right
JbB			rs.b	1		; Bit B
JbC			rs.b	1		; Bit C
JbA			rs.b	1		; Bit A
JbS			rs.b	1		; Bit start
JbZ			rs.b	1		; Bit Z
JbY			rs.b	1		; Bit Y
JbX			rs.b	1		; Bit X
JbM			rs.b	1		; Bit mode

J_U			EQU	(1<<JbU)	; Up
J_D			EQU	(1<<JbD)	; Down
J_L			EQU	(1<<JbL)	; Left
J_R			EQU	(1<<JbR)	; Right
J_B			EQU	(1<<JbB)	; B
J_C			EQU	(1<<JbC)	; C
J_A			EQU	(1<<JbA)	; A
J_S			EQU	(1<<JbS)	; Start
J_Z			EQU	(1<<JbZ)	; Z
J_Y			EQU	(1<<JbY)	; Y
J_X			EQU	(1<<JbX)	; X
J_M			EQU	(1<<JbM)	; Mode

IObTH			EQU	6		; TH pin bit
IObTR			EQU	5		; TR pin bit
IObTL			EQU	4		; TL pin bit
IO_TH			EQU	1<<IObTH	; TH pin
IO_TR			EQU	1<<IObTR	; TR pin
IO_TL			EQU	1<<IObTL	; TL pin

; -------------------------------------------------------------------------
