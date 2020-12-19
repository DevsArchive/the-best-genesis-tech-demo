
; -------------------------------------------------------------------------
;
;	Genesis Tech Demo
;		By Ralakimus 2018
;
;	File:		config.asm
;	Contents:	Configuration
;
; -------------------------------------------------------------------------

; -------------------------------------------------------------------------
; Header
; -------------------------------------------------------------------------

; Game name
GAME_NAME	EQUS	"MARIO TAKES A PISS"
; I/O support
IO_SUPPORT	EQUS	"J"
; SRAM support
SRAM_SUPPORT	EQU	$20202020
; SRAM start address
SRAM_START	EQU	$200000
; SRAM end address
SRAM_END	EQU	$200000
; Regions allowed
REGIONS		EQU	%1111
; Notes
NOTES		EQUS	"IT'S TIME TO TAKE A PISS!"

; -------------------------------------------------------------------------
; Flags
; -------------------------------------------------------------------------

; Enable lockout code
LOCKOUT		EQU	0
; Debug build flag
DEBUG		EQU	0

; -------------------------------------------------------------------------
; User defined
; -------------------------------------------------------------------------

; Insert user defined flags here

; -------------------------------------------------------------------------
