#import "helper.asm"


*=$0801 "Basic Program"
BasicUpstart($0810)

*=$0810 "Program"

start:
ClearScreen($0400,$20)

SetBorderColor(0)
SetBackgroundColor(LIGHT_GREY)

        lda #$1b             
        sta $d011
        lda #$08
        sta $d016                
        lda #$14              
        sta $d018             
        lda #$17                
        sta $dd00 

// main loop of the animation---------------------------------------------
		jsr setup_sprites
mloop:  jsr joystick
		jsr print_joystick
		jsr move_man
		jsr delay
		jsr expand_sprites
		jmp mloop
//------------------------------------------------------------------------

setup_sprites: 
        lda #$0c
        sta $d027               // grey color for sprite-man
        lda #%00000001          // set multicolor bit
        sta $d01c
        lda #$00                // multicolor register
        sta $d025               // black 
        lda #$01 				// and white
        sta $d026

        lda #$81               		 // set sprite pointer --> $2000 (128 x 64)
        sta sprite1_data_pointer  	// sprite data pointer           

        lda #$30
        sta spritepos
        lda #$90
        sta spritepos+1

        lda #%00000000          // expand sprites 
        sta $d017               // y direction   -> except the ball
        lda #%00000000
        sta $d01d               // x direction -> only goal sprites
        
        lda #%00000001
        sta $d015               // enable sprite 1
 
        lda #$00
        sta right_run_index
        sta left_run_index

        rts
// joystick control routine------------------------------------
joystick:
		lda #$00
		sta joy_value
		sta joy_fire

		lda $dc00
up:		lsr
		bcs down
		ldx #$04
		stx joy_value
		ldy #$01
		sty lockjump
down:	lsr 
left:	lsr 
		bcs right
		inc joy_value
		inc joy_value
right:	lsr 
		bcs fire
 		inc joy_value
fire:	lsr 
		bcs exitjoy
		ldx #$01
		stx joy_fire
exitjoy:rts		

print_joystick:
		lda joy_value	
		clc 
		adc #$30
		sta $0400
		rts
// sprite moving routine-----------------------------------------
move_man:
		lda joy_value
		cmp #$00 		// if zero, no movement
		bne !loop+
		rts

!loop:	cmp #$01 		//right
		bne !loop+
		jmp run_right
!loop:	cmp #$02 		//left
		bne !loop+
		jmp run_left
!loop:	cmp #$04 		//up
		bne !loop+
		jmp jump_up
!loop:	cmp #$05 		//up right
		bne !loop+
 		jmp jump_up_right
!loop:	cmp #$06 		//up left
		bne !loop+
		jmp jump_up_left
!loop:	rts

run_right:
		ldx right_run_index
		lda rightruntable,x
		sta sprite1_data_pointer
		inc spritepos
		inc right_run_index
		cpx #$0c
		bne !loop+
		ldx #$00
		stx right_run_index
!loop:	jmp move_exit

run_left:
		ldx left_run_index
		lda leftruntable,x
		sta sprite1_data_pointer
		dec spritepos
		inc left_run_index
		cpx #$0c
		bne !loop+
		ldx #$00
		stx left_run_index
!loop:	jmp move_exit

jump_up:







		rts

jump_up_right:
		rts

jump_up_left:
		rts


move_exit:
		rts

// adjust MSB of sprites------------------------------------------------------
expand_sprites:
        ldx #$00
!loop:  lda spritepos+$01,X     
        sta $D001,X
        lda spritepos+$00,X
        asl                 
        ror $D010             
        sta $D000,x
        inx
        inx
        cpx #$10
        bne !loop-
        rts

// delay routine------------------------------------------------------------
delay:
		lda #$f0 
!loop:	cmp $d012
		bne !loop-
		rts 


//vairables----------------------------------------------------
.label spritepos=$0370 
joy_value:	.byte $0
joy_fire:	.byte $0
screen:		.byte $0

//right run pointers

rightruntable:	.byte $80,$80,$80,$81,$81,$81,$82,$82,$82,$81,$81,$81
leftruntable:	.byte $84,$84,$84,$85,$85,$85,$86,$86,$86,$85,$85,$85
upjumptable:	.byte $83
rightjumptable:	.byte $83
leftjumptable:	.byte $87
jumpuptable:	.byte $3,$2,$1,$1,$1
jumpdowntable:	.byte $1,$1,$1,$2,$3
lockjump:		.byte $00



right_run_index:	.byte $00
left_run_index:		.byte $00
jump_up_index:		.byte $00
jump_down_index:	.byte $00
temp:				.byte $00
temp2:				.byte $00


.label sprite1_data_pointer	= $07f8


*=$2000

//spr_img0
// right 1 = $80
.byte $00,$00,$00,$00,$54,$00,$00,$6a,$00,$04,$65,$00,$01,$69,$00,$00
.byte $14,$00,$00,$14,$00,$00,$3c,$00,$00,$fe,$00,$00,$9b,$00,$03,$5f
.byte $40,$03,$5b,$50,$09,$17,$10,$0d,$15,$00,$04,$19,$00,$00,$25,$00
.byte $00,$55,$40,$00,$92,$40,$00,$51,$40,$03,$40,$70,$03,$c0,$f0,$8c

//spr_img1
// right 2 = $81
.byte $00,$54,$00,$00,$6a,$00,$00,$65,$00,$00,$69,$00,$00,$56,$00,$00
.byte $54,$00,$00,$3c,$00,$00,$3f,$00,$00,$ef,$00,$00,$77,$00,$00,$7e
.byte $00,$00,$77,$00,$00,$65,$00,$00,$35,$00,$00,$19,$00,$00,$15,$00
.byte $00,$19,$00,$00,$24,$00,$00,$14,$00,$00,$34,$00,$00,$3f,$00,$8c

//spr_img2
// right 3 = $82
.byte $00,$00,$00,$00,$54,$00,$00,$6a,$00,$00,$65,$00,$00,$69,$00,$01
.byte $16,$00,$00,$14,$00,$00,$3c,$00,$00,$2f,$00,$00,$7f,$00,$01,$67
.byte $80,$01,$7d,$f0,$05,$3d,$70,$04,$15,$10,$00,$19,$00,$00,$15,$00
.byte $00,$66,$40,$00,$51,$80,$00,$51,$40,$03,$40,$70,$03,$c0,$f0,$8c

//spr_img3
// jump right (up) = $83
.byte $00,$00,$00,$00,$54,$00,$04,$6a,$00,$01,$65,$00,$01,$69,$00,$00
.byte $14,$00,$00,$14,$00,$00,$3c,$00,$00,$bb,$00,$00,$df,$04,$0e,$5b
.byte $54,$05,$5f,$54,$04,$16,$00,$00,$15,$00,$00,$15,$00,$01,$55,$4c
.byte $0f,$50,$5c,$0d,$40,$7c,$0c,$00,$00,$00,$00,$00,$00,$00,$00,$8c

//spr_img4
// left 1 = $84
.byte $00,$00,$00,$00,$15,$00,$00,$a9,$00,$00,$59,$10,$00,$69,$40,$00
.byte $14,$00,$00,$14,$00,$00,$3c,$00,$00,$bf,$00,$00,$e6,$00,$01,$f5
.byte $c0,$05,$e5,$c0,$04,$d4,$60,$00,$54,$70,$00,$64,$10,$00,$58,$00
.byte $01,$55,$00,$01,$86,$00,$01,$45,$00,$0d,$01,$c0,$0f,$03,$c0,$8c

//spr_img5
// left 2 = $85
.byte $00,$15,$00,$00,$a9,$00,$00,$59,$00,$00,$69,$00,$00,$95,$00,$00
.byte $15,$00,$00,$3c,$00,$00,$fc,$00,$00,$fb,$00,$00,$dd,$00,$00,$bd
.byte $00,$00,$dd,$00,$00,$59,$00,$00,$5c,$00,$00,$64,$00,$00,$54,$00
.byte $00,$64,$00,$00,$18,$00,$00,$14,$00,$00,$1c,$00,$00,$fc,$00,$8c

//spr_img6
//left 3 = $86
.byte $00,$00,$00,$00,$15,$00,$00,$a9,$00,$00,$59,$00,$00,$69,$00,$00
.byte $94,$40,$00,$14,$00,$00,$3c,$00,$00,$f8,$00,$00,$fd,$00,$02,$d9
.byte $40,$0f,$7d,$40,$0d,$7c,$50,$04,$54,$10,$00,$64,$00,$00,$54,$00
.byte $01,$99,$00,$02,$45,$00,$01,$45,$00,$0d,$01,$c0,$0f,$03,$c0,$8c

//spr_img7
// jump left (up) = $87
.byte $00,$00,$00,$00,$15,$00,$00,$a9,$10,$00,$59,$40,$00,$69,$40,$00
.byte $14,$00,$00,$14,$00,$00,$3c,$00,$00,$ee,$00,$10,$f7,$00,$15,$e5
.byte $b0,$15,$f5,$50,$00,$94,$10,$00,$54,$00,$00,$54,$00,$31,$55,$40
.byte $35,$05,$f0,$3d,$01,$70,$00,$00,$30,$00,$00,$00,$00,$00,$00,$8c