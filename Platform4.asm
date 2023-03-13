#import "helper.asm"

.label spritepos	=$0370 
.eval var Player_x = spritepos
.eval var Player_y = spritepos+1
.label sprite1_data_pointer	= $07f8

*=$0801 "Basic Program"
BasicUpstart($0820)

*=$0820 "Program Listing"

start:
ClearScreen($0400)

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

			ldx #$00
!loop:		lda #$46
			sta $0630,x 
			inx 
			cpx #$28
			bne !loop-

			jsr setup_sprites

// main loop of the animation---------------------------------------------
mloop: 	jsr Player_Walk
		jsr JumpAndFall
		jsr expand_sprites
		jsr delay
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

	        lda #$81               		// set sprite pointer --> $2000 (128 x 64)
	        sta sprite1_data_pointer  	// sprite data pointer = $07f8          

	        lda #$30
	        sta Player_x 			// player x coordinate
	        lda #$90
	        sta Player_y			// player y coordinate

	        lda #%00000000          // expand sprites 
	        sta $d017               // y direction   -> except the ball
	        lda #%00000000
	        sta $d01d               // x direction -> only goal sprites
	        
	        lda #%00000001
	        sta $d015               // enable sprite 1

			lda #[STATE_FACE_RIGHT + STATE_WALK_RIGHT]
			sta Player_State        
	 
	        rts

// joystick control routine------------------------------------
Player_Walk: {
			lda JOY_PORT_2
			sta JOY_ZP
			lda JOY_ZP
			cmp #$7f
			bcc !up+
			lda #$01
			sta Player_WalkIndex_L
			sta Player_WalkIndex_R
			// PrintByteScreen(Player_WalkIndex_L,5,5,1)
			// PrintByteScreen(Player_WalkIndex_R,5,5,1)

			jmp !SkipMovement+

!up:		lda Player_State	// if player is already jumping or falling
			and #[STATE_FALL + STATE_JUMP]	// then don't jump or fall again
			bne !+
			lda JOY_ZP			// check joystick
			and #JOY_UP			// for up position
			bne !+ 
			lda Player_State	
			ora #STATE_JUMP		// set jump =1
			sta Player_State	// for the player state
			lda #$00
			sta Player_JumpIndex
			jmp !left+
!:

!left:		lda JOY_ZP			//check 00000100 -> left =0
			and #JOY_LT			// check against 1
			beq !Skip+			// 1&0=0  if result is 0, then move left
			jmp !right+ 		// otherwise, check right

!Skip:		sec 
			lda Player_x 		// move player left in accordance with walkspeed
			sbc Player_WalkSpeed
			sta Player_x	

			//Check screen edge
			lda Player_x
			cmp #LEFT_SCREEN_EDGE
			bcs !SkipEdgeCheck+
			lda #LEFT_SCREEN_EDGE
			sta Player_x

!SkipEdgeCheck:	
			lda Player_State
			and #[STATE_FALL + STATE_JUMP]
			beq !PlayerDone+ 
			lda PlayerJumpLSprite
			sta sprite1_data_pointer
			jmp !SkipMovement+

!PlayerDone:	
			lda Player_State
			and #[255 - STATE_FACE_RIGHT - STATE_WALK_RIGHT]
			ora #[STATE_WALK_LEFT + STATE_FACE_LEFT]
			sta Player_State
			inc Player_WalkIndex_L
			lda Player_WalkIndex_L
			cmp #$0c
			bne !+
			lda #$00
			sta Player_WalkIndex_L

!:			ldx Player_WalkIndex_L
			lda Player_L_table,x 
			sta sprite1_data_pointer
			jmp !SkipMovement+

!right:		lda JOY_ZP
			and #JOY_RT
			beq !Skip+
			jmp !SkipMovement+

!Skip:		clc 
			lda Player_x
			adc Player_WalkSpeed
			sta Player_x

			lda Player_x
			cmp #RIGHT_SCREEN_EDGE
			bcc !SkipEdgeCheck+
			lda #RIGHT_SCREEN_EDGE
			sta Player_x

!SkipEdgeCheck:

			lda Player_State
			and #[STATE_FALL + STATE_JUMP]
			beq !PlayerDone+ 
			lda PlayerJumpRSprite
			sta sprite1_data_pointer
			jmp !SkipMovement+

!PlayerDone:	
			lda Player_State
			and #[255 - STATE_FACE_LEFT - STATE_WALK_LEFT]
			ora #[STATE_WALK_RIGHT + STATE_FACE_RIGHT]
			sta Player_State
			inc Player_WalkIndex_R 
			lda Player_WalkIndex_R
			cmp #$0c  
			bne !+
			lda #$00
			sta Player_WalkIndex_R
!:			ldx Player_WalkIndex_R
			lda Player_R_table,x 
			sta sprite1_data_pointer

!SkipMovement:
			
			PrintByteScreen(Player_State,1,1,1)
			rts 
}

//-------------------------------------------------------------
JumpAndFall: {

			lda Player_State
			and #STATE_JUMP
			beq !Skip+

			// this part is jump
			lda Player_JumpIndex
			tax
			lda Player_y
			sec 
			sbc JumpTable,x
			sta Player_y
			inx 
			cpx #$10
			bne !+
			lda Player_State
			and #[255 - STATE_JUMP]
			ora #[STATE_FALL]
			sta Player_State
			ldx #$00
	!:		txa
			sta Player_JumpIndex
			PrintByteScreen(Player_JumpIndex,15,0,1)

	!Skip:	// this part is fall back
			lda Player_State
			and #STATE_FALL
			beq !Exit+

			lda Player_FallIndex
			tax
			lda Player_y
			clc 
			adc FallTable,x
			sta Player_y
			inx 
			cpx #$10
			bne !+
			lda Player_State
			and #[255 - STATE_FALL]
			sta Player_State
			lda Player_State
			cmp #$14
			bne !yl+
			ldy #$85
			sty sprite1_data_pointer
	!yl:	cmp #$28
			bne !yr+
			ldy #$81
			sty sprite1_data_pointer

	!yr:	ldx #$00
	!:		txa
			sta Player_FallIndex
			PrintByteScreen(Player_FallIndex,19,0,0)

!Exit:
			rts
}

// adjust MSB of sprites------------------------------------------------------
expand_sprites:
       		ldx #$00
!loop:  	lda spritepos+$01,x   
        	sta $D001,x 
        	lda spritepos+$00,x 
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
!loop:		cmp $d012
			bne !loop-
			rts 

//variables----------------------------------------------------

.label STATE_JUMP 		= %00000001  // 1
.label STATE_FALL 		= %00000010  // 2 			
.label STATE_WALK_LEFT  = %00000100  // 4  			// walk left = 4 + 16 =20    % 0001 0100   $14
.label STATE_WALK_RIGHT = %00001000  // 8 			// walk right = 8 + 32 = 40  % 0010 1000   $28
.label STATE_FACE_LEFT  = %00010000  //16 
.label STATE_FACE_RIGHT = %00100000  //32
.label JOY_UP 			= %00000001
.label JOY_DN 			= %00000010
.label JOY_LT 			= %00000100
.label JOY_RT 			= %00001000
.label JOY_FR 			= %00010000
.label JOY_PORT_2 		= $dc00
.label LEFT_SCREEN_EDGE  = $0a
.label RIGHT_SCREEN_EDGE = $a2
Player_JumpIndex: 	.byte $00
Player_FallIndex:	.byte $00
Player_WalkIndex_R:	.byte $00
Player_WalkIndex_L:	.byte $00
Player_State: 		.byte $00
Player_WalkSpeed:	.byte $01
JOY_ZP: 			.byte $00 

Player_R_table:		.byte $80,$80,$80,$81,$81,$81,$82,$82,$82,$81,$81,$81
Player_L_table:		.byte $84,$84,$84,$85,$85,$85,$86,$86,$86,$85,$85,$85
PlayerJumpRSprite:	.byte $83
PlayerJumpLSprite:	.byte $87

JumpTable:			.byte $04, $04, $04, $03, $03, $03, $02, $02, $02, $02, $01, $01, $01, $01, $01, $00

FallTable:			.byte $00, $01, $01, $01, $01, $01, $02, $02, $02, $02, $03, $03, $03, $04, $04, $04


Floor:	.text "________________________________________"


*=$2000 "Sprite data"

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