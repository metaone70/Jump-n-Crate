#import "helper.asm"

.label spritepos			= $0370 
.label SCREEN_LOOKUP 		= $fb 		
.label BelowCharVal  		= $0383
.label sprite1_data_pointer	= $07f8
.eval var Player_x 			= spritepos
.eval var Player_y 			= spritepos+1
.label SPBACKGR_COLL_REG 	= $d01f

*=$0801 "Basic Program"
BasicUpstart($0810)

*=$0810 "Program Listing"


SetBorderColor(BLACK)
SetBackgroundColor(LIGHT_GREY)
ClearScreen($0400,$20)

	    lda #%00011011   	// %0001 1000   Bit #3: 1 = 25 rows,  Bit #4: 1 = Screen on   
	    sta $d011

	    lda #%00001000 		// %0001 1000 Bit #3:1 = 40 columns,  Bit #4: 1 = Multicolor mode on
	    sta $d016                

	    lda #%00010100     	// %0001 0100 bit1-3=text character address 	%110, 6: $3000-$37FF
	    sta $d018        	// Values %010 and %011 in VIC bank #0 and #2 select Character ROM instead.
	        					// 		 bit4-7=video matrix base address  %0001, 1: $0400-$07FF

	    lda #%00010111     	// %0001 0111 bits0&1=select 16K bank for VICII %11, 3: Bank #0, $0000-$3FFF, 0-16383.   
	    sta $dd00 

		ldx #$00 				// draw the platforms to specific rows
!loop:	lda Floor0,x 			
		sta $0568,x 
		lda Floor1,x
		sta $0608,x 
		lda Floor2,x 
		sta $06a8,x 
		lda #$02
		sta $d968,x 
		sta $da08,x 
		sta $daa8,x 
		inx 
		cpx #$28
		bne !loop-

setup_sprites:   
        lda #$0c
        sta $d027               	// grey color for sprite-man
        lda #%00000001         		// set multicolor bit
        sta $d01c
        lda #$00              	  	// multicolor register
        sta $d025             		// black 
        lda #$01 					// and white
        sta $d026

        lda #$81               		// set sprite pointer --> $2000 (128 x 64)
        sta sprite1_data_pointer  	// sprite data pointer = $07f8          

        lda #$4c
        sta Player_x 				// player x coordinate
        lda #$70
        sta Player_y				// player y coordinate

        lda #%00000001
        sta $d015               	// enable sprite 1

        lda #$20
        sta Space_Value

		lda #[STATE_FACE_RIGHT + STATE_WALK_RIGHT]	// set initial direction
		sta Player_State        
	 	
	 	jsr START_WITH_FALLING			// drop down the character

        // sei 					// start interrup routine
        // lda #$7f
        // sta $dc0d
        // lda $d01a
        // ora #$01
        // sta $d01a
        // lda $d011
        // and #$7f
        // sta $d011
        // lda #<mloop
        // sta $0314
        // lda #>mloop
        // sta $0315
        // cli
        // rts

// main loop of the animation---------------------------------------------
mloop: 	
		//inc $d019
		jsr Player_Walk
		jsr CalcSpriteChar
		jsr JumpAndFall
		jsr Expand_Sprites
		jsr delay
		jsr PrintData2Screen
		//jmp $ea31
		jmp mloop
//------------------------------------------------------------------------

// joystick control routine------------------------------------
Player_Walk: {

		lda JOY_PORT_2					// load Joyport2 state
		sta JOY_ZP						// and save it to variable
		lda JOY_ZP
		cmp #%01111111					// all on means no switch activated,so less then $7f
		bcc !up+						// indicates there is a movement 
		lda #$01 						// if there is no mvement
		sta Player_WalkIndex_L			// load the standing sprite index
		sta Player_WalkIndex_R			//and skip movement
		jmp !SkipMovement+

!up:	lda Player_State				// if player is already jumping or falling
		and #[STATE_FALL + STATE_JUMP]	// then don't jump or fall again
		bne !+
		lda JOY_ZP						// check joystick
		and #JOY_UP						// for up position
		bne !+ 
		lda Player_State	
		ora #STATE_JUMP					// set jump =1 (switch on the bit)
		sta Player_State				// for the player state
		lda #$00 						// and reset the Jump index
		sta Player_JumpIndex
		jsr JumpSound
		jmp !left+ 						// and then continu checking other directions
!:

!left:	lda JOY_ZP						//check 00000100 -> left =0
		and #JOY_LT						// check against 1
		beq !Skip+						// 1&0=0  if result is 0, then move left
		jmp !right+ 					// otherwise, check right

!Skip:		sec 
		lda Player_x 					// move player left in accordance with walkspeed
		sbc Player_WalkSpeed
		sta Player_x	

		lda Player_State				// check also if the player is in air
		and #STATE_JUMP	
		bne !EdgeCheck+					// if no (=on floor), go and do the edge check

		lda Player_State				// check also if the player is in air
		and #STATE_FALL	
		bne !EdgeCheck+					// if no (=on floor), go and do the edge check

		jsr GetBelowChar				// otherwise, check if the character in on floor
		lda BelowCharVal				// look at below character
		cmp Space_Value					// is it floor? If yes, then continue walking 
		bne !EdgeCheck+	  	   			 // and check screen edges

						// if you are here, there is no jump, player walking 
						// and there is no floor beneath the player
		lda Player_State				// so make the state revision
		ora #STATE_FALL					// and start fall state (switch on fall state)
		sta Player_State
		lda #$00 						// reset the fall index
		sta Player_FallIndex  			// at this point, state = left & falling (18=$12)
		jsr FallSound					// or left & walking & falling (22=$16 ) 

		//Check screen edge
!EdgeCheck:	lda Player_x
		cmp #LEFT_SCREEN_EDGE			// did he reach the left edge coordinate?
		bcs !SkipEdgeCheck+				// if no, skip
		lda #LEFT_SCREEN_EDGE			// if yes, limit the coordinate
		sta Player_x

!SkipEdgeCheck:	
		lda Player_State				// check player state whether he
		and #[STATE_FALL + STATE_JUMP]	// is falling or jumping
		beq !PlayerDone+ 				// if not, go progress walking
		lda Player_State				// if falling
		and #STATE_FALL
		beq !+
		lda PlayerJumpLSprite			// load the falling sprite 
		sta sprite1_data_pointer		// (standing and facing left)
		jmp !++							// and finish moving
	!:	lda PlayerJumpLSprite			// or load the jumping sprite
		sta sprite1_data_pointer		

	!:	jmp !SkipMovement+				// and finish moving


!PlayerDone:	
		// this part is walking left
		lda Player_State				// ensure that the player state is face left & walk left
		and #[255 - STATE_FACE_RIGHT - STATE_WALK_RIGHT]
		ora #[STATE_WALK_LEFT + STATE_FACE_LEFT]
		sta Player_State
		inc Player_WalkIndex_L			// increment walking left index
		lda Player_WalkIndex_L			// for the right sprite frame
		cmp #$0c						// did it reach 12?
		bne !+ 							// if no, continue with the sprite
		lda #$00 						// if yes, then reset it
		sta Player_WalkIndex_L

!:		ldx Player_WalkIndex_L			// load the index
		lda Player_L_table,x 			// get the number from the table in accordance with the index
		sta sprite1_data_pointer		// save it to the sprite pointer for the frame
		jsr Walking_Sound_Left			// play the sound effect
		jmp !SkipMovement+				// left walking completed, exit the checking routine

!right:	lda JOY_ZP						//check 00001000 -> right =0
		and #JOY_RT 					// check against 1
		beq !Skip+ 						// 1&0=0  if result is 0, then move right
		jmp !SkipMovement+ 				// otherwise finish movement routine

!Skip:	clc 							// move player right in accordance with walkspeed
		lda Player_x
		adc Player_WalkSpeed
		sta Player_x	

		lda Player_State				// check also if the player is in air
		and #STATE_JUMP	
		bne !EdgeCheck+					// if no (=on floor), go and do the edge check

		lda Player_State				// check also if the player is in air
		and #STATE_FALL 	
		bne !EdgeCheck+					// if no (=on floor), go and do the edge check

		jsr GetBelowChar				// otherwise, check if the character in on floor
		lda BelowCharVal				//look at below character
		cmp Space_Value					// is it floor?
		bne !EdgeCheck+	 	  			// if no, then check screen edges

						// if you are here, there is no jump and 
						// there is no floor beneath the player
		lda Player_State				// so make the state revision
		ora #STATE_FALL					// and start fall state  (switch on fall state)
		sta Player_State
		lda #$00 						// reset the fall index
		sta Player_FallIndex 			// at this point, state = right & falling (34=$22)
		jsr FallSound					// or right & walking & falling (40=$28 ) 

		//Check screen edge
!EdgeCheck:	
		lda Player_x
		cmp #RIGHT_SCREEN_EDGE			// did he reach the right edge coordinate?
		bcc !SkipEdgeCheck+				// if no, skip
		lda #RIGHT_SCREEN_EDGE			// if yes, limit the coordinate
		sta Player_x

!SkipEdgeCheck:
		lda Player_State				// check player state whether he
		and #[STATE_FALL + STATE_JUMP]	// is falling or jumping
		beq !PlayerDone+ 				// if not, go progress walking
		lda Player_State				// check the state
		and #STATE_FALL
		beq !+							
		lda PlayerJumpRSprite			// if falling, load the falling sprite 
		sta sprite1_data_pointer		// (standing and facing left)
		jmp !++							// and finish moving
!:		lda PlayerJumpRSprite			// or load the jumping sprite
		sta sprite1_data_pointer		// and save it to the sprite pointer

!:		jmp !SkipMovement+				// and finish moving			

		// this part is walking right
!PlayerDone:	
		lda Player_State				// ensure that the player state is face right & walk right
		and #[255 - STATE_FACE_LEFT - STATE_WALK_LEFT]
		ora #[STATE_WALK_RIGHT + STATE_FACE_RIGHT]
		sta Player_State
		inc Player_WalkIndex_R 			// increment walking right index
		lda Player_WalkIndex_R			// for the right sprite frame
		cmp #$0c  						// did it reach 12?
		bne !+							// if no, continue with the sprite
		lda #$00 						// if yes, then reset it
		sta Player_WalkIndex_R			
!:		ldx Player_WalkIndex_R			// load the index
		lda Player_R_table,x 			// get the number from the table in accordance with the index
		sta sprite1_data_pointer		// save it to the sprite pointer for the frame
		jsr Walking_Sound_Right			// play the sound effect
!SkipMovement:
		rts 
}

//-------------------------------------------------------------
JumpAndFall: {

		lda Player_State				// check the player state
		and #STATE_JUMP					// if there is no jump
		beq !Skip+						// then skip to the fall part

		// this part is jump
		lda Player_JumpIndex			// load the jump index
		tax
		lda Player_y					// get the player y coordinate
		sec 
		sbc JumpTable,x 				// decrement the y coordinate by the jump table value
		sta Player_y					// and store it to the player y coordinate
		inx 							// increment the jump index
		cpx #$12 						// check if it reached 12 (limit of jump index)
		bne !+ 							// if no, continue
		lda Player_State 				// if yes, jump will be finalized
		and #[255 - STATE_JUMP] 		// remove jump state from player state
		ora #[STATE_FALL]				// and start fall sequence
		sta Player_State
		ldx #$00 						// and reset the jump index (if completed)
!:		txa
		sta Player_JumpIndex			// save the current index

!Skip:	// this part is fall back
		lda Player_State				// check the player state
		and #STATE_FALL					// if there is no fall
		beq !Exit+						// then skip to the ending part

		jsr GetBelowChar
		lda BelowCharVal				//look at below character
		cmp Space_Value					// is it floor?
		bne !FinJ+						// if yes, then finalize jump

		lda Player_FallIndex			// otherwise continue fall
		tax
		lda Player_y					// get the player y coordinate
		clc 
		adc FallTable,x 				// increment the y coordinate by the jump table value 
		sta Player_y					// and store it to the player y coordinate
		inx 							// increment the jump index
		cpx #$12 						// check if it reached 12 (limit of jump index)
		bne !++ 						// if no, continue and exit by storing the fall index

					// if you are here, the fall from the jump has ended
					// but there is no floor beneath, so fall down until there is floor
!:		jsr delayfast 					// insert a delay, since this loop is out of the main delay routine
		inc Player_y 					// increment player y coordinate
		jsr CalcSpriteChar				// get the character coordinates of the player
		jsr Expand_Sprites				// calculate the final (hardware) sprite coordinates
		jsr GetBelowChar 				// loot at the below character
		lda BelowCharVal 					
		cmp Space_Value 				// if it is space, rinse and repeat
		beq !- 							// until there is floor

		//fall completed. load the sprite for standing still
!FinJ:	lda Player_State 				//load the player state
		and #[255 - STATE_FALL] 		// remove the fall state from it
		sta Player_State 				// and store it to the palyer state
		lda Player_State 				// check the final state
		cmp #$14 						// check if it is $14, it is walking left and facing left
		bne !yl+ 						// if no, skip to checking right
		ldy PlayerStandLSprite			// if yes, load the standing left postion sprite
		sty sprite1_data_pointer		// and store it to the sprite pointer
!yl:	cmp #$28 						// check if it is $28, it is walking right and facing right
		bne !yr+						// if no, skip to storing fall index
		ldy PlayerStandRSprite 			// if yes, load the standing right postion sprite
		sty sprite1_data_pointer		// and store it to the sprite pointer

!yr:	ldx #$00 						// if you are here, then reset the fall index
!:		txa
		sta Player_FallIndex 			// store it to the fall index

!Exit:
		rts
}

// adjust MSB of sprites------------------------------------------------------
Expand_Sprites:	{
       		ldx #$00 					// load the sprite y position
!loop:  	lda spritepos+$01,x   		// and store it to the hardware sprite register
        	sta $d001,x 
        	lda spritepos+$00,x 		// load the sprite x position
        	asl                 		// implement Arithmetic shift right
        	ror $d010             		// and expand MSB
        	sta $d000,x 				// and store it back to the register
        	inx 						// increment the index by 2 (y and x registers)
        	inx
        	cpx #$10 					// and do it for all sprites
        	bne !loop-
	       	rts
}


// Sprite falling at the beginning of the scene
START_WITH_FALLING: {

		jsr FallSound					// play the fall sound fx
!:		jsr delayfast					// insert a (relative) faster delay
		jsr CalcSpriteChar				// calculate the char location of the sprite
		inc Player_y					// increment the y position
		jsr Expand_Sprites				// place the sprite in accordance with MSB value
		jsr GetBelowChar				// look at beneath the player character
		lda BelowCharVal				
		cmp Space_Value					// if it is space, rinse and repeat
		beq !-							// otherwise, finalize the fall
		rts 			
}

// Calculate sprite character positions--------------------------------------
CalcSpriteChar: {
		// sprite positions 

		lda Player_x 					// get the x sprite coordinate
		cmp #$83 						// there is flaw in the program. It resets the character
		bcs !+							// value around here ($83), so there is a check for it
		sec 							// if below $83
		sbc #$07 						// then subtract $7 (by trial and error)
		lsr  							// divide it by four (x coordinate is half, so div by 4 is needed)
		lsr 
		sta SpriteCharX 				// store it to character x variable
		jmp !++

!:		asl 							// otherwise, it is below $83
		sec  							
		sbc #$07 						// so subtract $7 (by trial and error)
		lsr  							// divide by 8 -> 1 character is 8 bits long
		lsr 
		lsr 
		clc
		adc #$20 						// add $20 for the left border
		sta SpriteCharX 				// store it to character x variable

!:		lda Player_y					// get the x sprite coordinate
		sec 
		sbc #$1b 						// subtract $1b for the top border (by trial and error)
		lsr  							// divide by 8 -> 1 character is 8 bits long
		lsr 
		lsr 
		sta SpriteCharY 				// store it to character y variable
		rts 
}

// delay routine------------------------------------------------------------
delay:
		waitForRasterLine($f0)
		rts 

delayfast:
		waitForRasterLine($10)
		rts 


// print data temporarily to screen-----------------------------------------
PrintData2Screen: {
		PrintByteScreen(Player_x,10,0,0)
		PrintByteScreen(Player_y,13,0,0)
		PrintByteScreen(SpriteCharX,10,1,2)
		PrintByteScreen(SpriteCharY,13,1,2)
		jsr GetBelowChar
		PrintByteScreen(BelowCharVal,20,0,0)	
		PrintByteScreen(Player_State,37,0,4)		
		rts 
}

// Get the character at given coordinates-----------------------------------
GetBelowChar: {
		// the screen coordinate table is formed via .fill command. The column addresses
		// are calculated there

		lda SpriteCharY 				// get sprite y char value
		tay
		lda ScreenLSB,y 				// load the yth value from screen LSB table
		sta SCREEN_LOOKUP 				// and store it to the ZP address
		lda ScreenMSB,y 				// load the yth value from screen LSB table
		sta SCREEN_LOOKUP + 1 			// and store it to the ZP+1 address
		lda SpriteCharX
		tay 							// get sprite y char value 
		lda (SCREEN_LOOKUP),y 			// load the value at the ZP address by adding the x value
		sta BelowCharVal 				// store the character value to the variable
		rts
	}

//Walking sounds------------------------------------------------------------
Walking_Sound_Left: {
		lda Player_WalkIndex_L			// load the left walking index
		cmp #$01 						// if it is 1  (of total 12)
		beq Step1Sound 					// then play the 1st step sound
		cmp #$06 						// if it is 6  (of total 12)
		beq Step2Sound 					// then play the 2nd step sound
		rts 							// so for 12 frames, only 2 times is sufficient
}

Walking_Sound_Right: {
		lda Player_WalkIndex_R
		cmp #$01
		beq Step1Sound
		cmp #$06
		beq Step2Sound
		rts 
}

Step1Sound: {
		sei
		lda #<Sound_IRQ
		sta $0314
		lda #>Sound_IRQ
		sta $0315
		lda #$0f 
		sta $d418
		lda #2
		ldy #1
		jsr $104a
		cli 
		rts 
Sound_IRQ:
		inc $d019	
		jsr $12a9 
		jmp $ea31
}

Step2Sound: {
		sei
		lda #<Sound_IRQ
		sta $0314
		lda #>Sound_IRQ
		sta $0315
		lda #$0f 
		sta $d418
		lda #3
		ldy #1
		jsr $104a
		cli 
		rts 
Sound_IRQ:
		inc $d019	
		jsr $12a9 
		jmp $ea31
}

JumpSound: {
		sei
		lda #<Sound_IRQ
		sta $0314
		lda #>Sound_IRQ
		sta $0315
		lda #$0f 
		sta $d418
		lda #0
		ldy #1
		jsr $104a
		cli 
		rts 
Sound_IRQ:
		inc $d019	
		jsr $12a9 
		jmp $ea31
}

FallSound:{
		sei
		lda #<Sound_IRQ
		sta $0314
		lda #>Sound_IRQ
		sta $0315
		lda #$0f 
		sta $d418
		lda #1
		ldy #1
		jsr $104a
		cli 
		rts 
Sound_IRQ:
		inc $d019	
		jsr $12a9 
		jmp $ea31
}


//variables----------------------------------------------------

.label STATE_JUMP 		= %00000001  // 1
.label STATE_FALL 		= %00000010  // 2 			
.label STATE_WALK_LEFT  = %00000100  // 4  	// walk left = 4 + 16 =20    % 0001 0100   $14
.label STATE_WALK_RIGHT = %00001000  // 8 	// walk right = 8 + 32 = 40  % 0010 1000   $28
.label STATE_FACE_LEFT  = %00010000  //16 
.label STATE_FACE_RIGHT = %00100000  //32
.label JOY_UP 			= %00000001
.label JOY_DN 			= %00000010
.label JOY_LT 			= %00000100
.label JOY_RT 			= %00001000
.label JOY_FR 			= %00010000
.label JOY_PORT_2 		= $dc00
.label LEFT_SCREEN_EDGE = $0a
.label RIGHT_SCREEN_EDGE= $a3
.label BORDER_COLOR		= $d020
.label BACKGROUND_COLOR	= $d021
.label MULTICOLOR_1		= $d022
.label MULTICOLOR_2		= $d023
.label MULTICOLOR_3		= $d024

Player_JumpIndex: 	.byte $00
Player_FallIndex:	.byte $00
Player_WalkIndex_R:	.byte $00
Player_WalkIndex_L:	.byte $00
Player_State: 		.byte $00
Player_WalkSpeed:	.byte $01
Collision_Temp:		.byte $00
JOY_ZP: 			.byte $00 
SpriteCharX:		.byte $00
SpriteCharY:		.byte $00
Space_Value:		.byte $00

Player_R_table:		.byte $80,$80,$80,$81,$81,$81,$82,$82,$82,$81,$81,$81
Player_L_table:		.byte $84,$84,$84,$85,$85,$85,$86,$86,$86,$85,$85,$85
PlayerJumpRSprite:	.byte $83
PlayerJumpLSprite:	.byte $87
PlayerStandRSprite:	.byte $81
PlayerStandLSprite:	.byte $85

JumpTable:		.byte $04, $04, $04, $04, $03, $03, $03, $03, $02, $02, $02, $02, $01, $01, $01, $01, $01, $00
FallTable:		.byte $00, $01, $01, $01, $01, $01, $02, $02, $02, $02, $03, $03, $03, $03, $04, $04, $04, $04
				.byte $04, $04, $04, $04, $04, $04, $04, $04

Floor0:			.byte $a0,$a0,$a0,$a0,$a0,$a0,$20,$20,$20,$20,$20,$20,$a0,$a0,$a0,$a0,$20,$20,$20,$20
				.byte $20,$20,$a0,$a0,$a0,$a0,$a0,$a0,$20,$20,$20,$20,$20,$20,$a0,$a0,$a0,$a0,$a0,$a0

Floor1:			.byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$20,$20,$20,$20,$20
				.byte $20,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0

Floor2:			.byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
				.byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0					

ScreenLSB:  	.fill 25, <[$0400 + i * $28]
ScreenMSB:		.fill 25, >[$0400 + i * $28]


// Sound effects file---------------------------------------------------------------------------

*=$1000
.import c64 "Sounds.prg"

// Sprite Data------------------------------------------------------------------------------------
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