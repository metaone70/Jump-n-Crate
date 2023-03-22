// Default-segment:
// $0801-$080c Basic Program
// $0810-$0bf4 Program Listing
// $2000-$222c PlayerControl
// $222d-$2337 MoveRobots
// $2338-$23cc CheckPlayerCollection
// $4400-$47e7 map_data
// $5000-$57ff charset_data
// $5800-$58ff charset_attrib_L1_data
// $6000-$637f Sprite data
// $c000-$c3f3 Sound Effects

.label SpritePos		= $0370 
.label SCREEN_LOOKUP 	= $f7 				// ($f7 + $f8)
.label SCREEN_LOOKUP2	= $f9 				// ($f9 + $fa)
.label CHARCOLORRAM		= $96 				// ($96 + $97)
.label TIME_LOOKUP		= $fb 				// ($fb + $fc)
.label TIME_COLOR_LOOKUP = $fd 				// ($fd + $fe)
.label BelowCharVal  	= $0383
.label OnCharVal		= $0384
.label Sprite_Pointer	= $47f8
.eval var Player_x 		= SpritePos
.eval var Player_y 		= SpritePos+1
.label SPBACKGR_COLL_REG 	= $d01f
.const TotalCrateNumber = 11

#import "helper.asm"
#import "PlayerControl.asm"
#import "MoveRobots.asm"
#import "CheckPlayerCollection.asm"

*=$0801 "Basic Program"
BasicUpstart($0810)

*=$0810 "Program Listing"
		sei 

		lda #$36			// disable BASIC ROM
      	sta $0001 			// in order to use more ZP

		SetBorderColor(BLACK)
		SetBackgroundColor(BLACK)

	  	lda #%00011011   	// %0001 1000   Bit #3: 1 = 25 rows,  Bit #4: 1 = Screen on   
	  	sta $d011

	   	lda #%00011000 		// %0001 1000 Bit #3:1 = 40 columns,  Bit #4: 1 = Multicolor mode on
	   	sta $d016                

	   	 lda #%00010100   // %0001 0100 bit 1-3=text character address 	%010, 2: $1000-$17FF ($4000+$1000=$5000)
	   	 sta $d018        // Values %010 and %011 in VIC bank #0 and #2 select Character ROM instead.
	        							// 		 bit 4-7=video matrix base address  %0001, %0001, 1: $0400-$07FF ($4400)

	   	lda #%00010110     // %0001 0111 bits 0&1=select 16K bank for VICII %10, 2: Bank #1, $4000-$7FFF
	   	sta $dd00 

	   	lda #LIGHT_GREY
	   	sta MULTICOLOR_1
	   	lda #WHITE			
	   	sta MULTICOLOR_2

	   	// this part is to set the color of multicolor characters
	   	// we first get the character # from secreen, we find its attrib 
	   	// from the attrib table and store it to its color RAM
	   	// and we do it 4 times (4x256=1000 characters)
	   	ldy #$00
	!:	lda SCREEN_ADDRESS,y 		// get the character value from screen position
		tax 
	   	lda $5800,x 				// get the attribute of the character
	   	sta COLOR_RAM,y 			// and store it to its color RAM
	   	lda SCREEN_ADDRESS+$100,y
	   	tax 
	   	lda $5800,x 
	   	sta COLOR_RAM+$100,y
	   	lda SCREEN_ADDRESS+$200,y
	   	tax 
	   	lda $5800,x 
	   	sta COLOR_RAM+$200,y
	   	lda SCREEN_ADDRESS+$300,y
	   	tax 
	   	lda $5800,x 
	   	sta COLOR_RAM+$300,y
	   	iny 
	   	bne !-

	   	lda Time_Max				// set the time to max at the beginning
	   	sta Time_Status

		lda #[STATE_FACE_LEFT + STATE_WALK_LEFT]	// set initial direction as facing right
		sta Player_State        

		lda Idle_Timeout
		sta Idle_Time

		lda #<TIMESTATUSADDR		// load the time status address to ZP
		sta TIME_LOOKUP
		lda #>TIMESTATUSADDR
		sta TIME_LOOKUP+1
		lda #<TIMESTATUSCOLOR		// load time status color to ZP
		sta TIME_COLOR_LOOKUP
		lda #>TIMESTATUSCOLOR
		sta TIME_COLOR_LOOKUP+1	

		jsr SetupSprites 				// setup sprites

// game interrupts
		sei 				// start interrup routine
		lda #$7f
		sta $dc0d
		lda $d01a
		ora #$01
		sta $d01a
		lda $d011
		and #$7f
		sta $d011
		lda #<mloop
		sta $0314
		lda #>mloop
		sta $0315
		cli
		// rts
		jmp *	

// main loop of the animation---------------------------------------------
mloop: 	
		inc $d019					// acknowledge VIC IRQ
		jsr $c2a9 					// setup sound routine
		jsr MoveRobots
		jsr Player_Walk
		jsr CalcSpriteChar
		jsr JumpAndFall
		jsr Check_Player_Collection
		jsr CheckPlayer_Collision
		jsr delay
		jsr Expand_Sprites
		jsr DecreaseTime
		jmp $ea31

// adjust MSB of sprites------------------------------------------------------
Expand_Sprites:	{
		lda #249								// wait for raster 249 to draw sprites
		sta $d012

	 	ldx #$00 								// load the sprite y position
!loop:  	
		lda SpritePos+$01,x   	// and store it to the hardware sprite register
		sta $d001,x 
	  	lda SpritePos+$00,x 		// load the sprite x position
	  	asl                 		// implement Arithmetic shift right
	  	ror $d010             	// and expand MSB
	  	sta $d000,x 						// and store it back to the register
	  	inx 										// increment the index by 2 (y and x registers)
	  	inx
	  	cpx #$10 								// and do it for all sprites
	  	bne !loop-
	  	rts
  }

// Calculate sprite character positions--------------------------------------
CalcSpriteChar: {
		lda Player_x
		sec 
		sbc #6
		lsr 
		lsr 
		sta SpriteCharX

!:	lda Player_y				// get the x sprite coordinate
		sec 
		sbc #29 						// subtract #29 for the top border and sprite offset
		lsr  							  // divide by 8 -> 1 character is 8 bits long
		lsr 
		lsr 
		sta SpriteCharY		  // store it to character y variable
		rts 
		}

// delay routine------------------------------------------------------------
delay:
		waitForRasterLine($f0)
		rts 

delayfast:
		waitForRasterLine($10)
		rts 


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
		tay 											// get sprite x char value 
		lda (SCREEN_LOOKUP),y 			// load the value at the ZP address by adding the x value
		sta BelowCharVal 				// store the character value to the variable
		rts
	}

AdjustSpriteYCoor: {					// to obtain pixel perfect landing
		lda SpriteCharY					// by doing the opposite of character calculation routine
		asl 
		asl 
		asl 
		clc 
		adc #29
		sta Player_y
		rts 
	}


//Get the character that the sprite is on-------------------------------------------------
GetOnChar: {
		lda SpriteCharY 						// get sprite y char value
		tay
		dey  												// we decrease it by 1, so the sprite char we are after is just above the floor line
		lda ScreenLSB,y 						// load the yth value from screen LSB table
		sta SCREEN_LOOKUP2 					// and store it to the ZP address
		lda ScreenMSB,y 						// load the yth value from screen LSB table
		sta SCREEN_LOOKUP2 + 1 			// and store it to the ZP+1 address

		lda SpriteCharX
		tay 												// get sprite x char value 
		lda (SCREEN_LOOKUP2),y 			// load the value at the ZP address by adding the x value
		sta OnCharVal 							// store the character value to the variable
		rts
	}


// Check player vs robots/bullets (sprite) collisions
CheckPlayer_Collision: {

		lda Collision_Flag			// check if there has been a collision
		cmp #$01 					// if yes, exit (there is a timeout)
		beq !Exit+

!:		lda $d01e					// check sprite to sprite collision register
		and #%00000001				// and check the player sprite collision
  		cmp #$01
		beq !+						// if result = 1, then collision occured
		jmp !Exit+ 
									// in order to prevent full degredationf of time due to collision
									// we are checking for a short counter time of 7 cycles		
!:		lda Collision_Timer			// check the collision timer if it is 7 (initial value)
		cmp #$07 					// if it is less than 7, then there is no need to decrease time status
		bne !+						
		jsr Decrease_Time_Status	// if it is 7, then first decrease the time status and then
!:		dec Collision_Timer			// decrease the collision timer
		lda Collision_Timer	
		cmp #$00 					// did the timer reached zero?
		bne !Exit+					// if not, exit
		lda #$07 					// if it did, then reset the timer to 7
		sta Collision_Timer
		lda #$00 					// and also reset the collision flag
		sta Collision_Flag

!Exit:	rts 
}


// Decrease time---------------------------------------------------------------
Decrease_Time_Status: {

		dec Time_Status 			// decrease the time status
		lda Time_Status 			// check status
		bne !+ 						// if not zero, skip next check
		jmp GameOver 				// if zero, go to Game Over section

									// $47 = empty char
									// $48 = 1/4 full char	
									// $49 = 1/2 full char	
									// $4a = 3/4 full char
									// $4b = full char	

!:		dec Time_Status_Char 		// decrease the bar by chaning the character
		lda Time_Status_Char		// check the current status character
		cmp #$47 					// if it is not empty, then continue printing
		bcs !print+
									// here, the status char is empty
		lda #$4b 					// so load the full char to status char
		sta Time_Status_Char
		dec $fb  					// decrement the LSB of status char, so it is one left char
		dec $fd 					// also decrement the color address LSB
		lda $fb 
		cmp #$b3 					// also check if we are at the last time status box
		bcs !print+ 				// if no, pass on to printing
		jmp GameOver 				// if yes, then the boxes are all empty, so finih the game 


!print:
		ldy #$00 					// load the status character
		lda Time_Status_Char 		// and save it to screen address via ZP
		sta (TIME_LOOKUP),y 

		ldx Time_Status_Char 		// same goes for the color address
		lda $5800,x 
		sta (TIME_COLOR_LOOKUP),y 

!Exit:	rts 

}

// Decrease time---------------------------------------------------------------
Increase_Time_Status: {

		lda Time_Status 			// check status
		cmp #Time_Max
		bcc !+
		lda Time_Max
		sta Time_Status

!:		lda $fb
		cmp #$be 
		bcc !+

		ldy #$00
		lda #$4b 				// full health character
		sta (TIME_LOOKUP),y 

		ldx Time_Status_Char
		lda $5800,x 
		sta (TIME_COLOR_LOOKUP),y 
		jmp !Exit+

!:		inc Time_Status
		inc Time_Status
		inc Time_Status
		inc Time_Status
		inc Time_Status

		ldy #$00
		lda #$4b 				// full health character
		sta (TIME_LOOKUP),y 

		ldx Time_Status_Char
		lda $5800,x 
		sta (TIME_COLOR_LOOKUP),y 

		inc $fb 
		inc $fd 
		ldy #$00
		lda Time_Status_Char
		sta (TIME_LOOKUP),y 

		ldx Time_Status_Char
		lda $5800,x 
		sta (TIME_COLOR_LOOKUP),y 

!Exit:	rts 

}

// Game Over ------------------------------------------------------
GameOver: {

	rts 


}



//Setup sprites------------------------------------------------------------------------------
SetupSprites: {
			lda #$ff         			// set multicolor bit
			sta $d01c					// for sprites 0-7

			lda #BLACK              	// multicolor register
			sta SPRITE_MULCOL_1     	// black 
			lda #WHITE					// and white
			sta SPRITE_MULCOL_2

			ldx #$00 								
!:			lda SpriteCoordinates,x 
			sta SpritePos,x 
			inx 
			cpx #$0f 					// do it for 8 sprites
			bne !-

			ldx #$00
!:			lda SpritePointers,x   		 // set sprite pointer --> VIC BANK + $2000 (128 x 64)
			sta Sprite_Pointer,x   
			lda SpriteColors,x 			// load sprite colors from table
			sta SPRITE_COLOR,x 			// store it to color registers
			inx 
			cpx #$08
			bne !-

			lda #%11111111
			sta $d015               	// enable all sprites
			rts 
}

// Increment score-----------------------------------------------------------
AddScore: {
		inc $47aa
		lda $47aa
		cmp #$25
		bne !+
		lda #$1b
		sta $47aa
		inc $47a9
		lda $47a9
		cmp #$25
		bne !+
		lda #$1b
		sta $47a9
		inc $47a8
!:		rts 
}

// After collecting all crates, the door will open--------------------------------------
OpenDoor: {

		PrintChar2Screen(0,$4400,36,20)
		PrintChar2Screen(0,$4400,36,21)
		jsr DoorOpeningSound
		rts 
}

// Decrease time as time goes by--------------------------------------------------------
DecreaseTime: {

		dec Time_Counter
		bne !Exit+
		jsr Decrease_Time_Status
		lda Time_Counter
		cmp #$00
		bne !Exit+
		lda #$80 
		sta Time_Counter

!Exit:

}





//Playing sound effects------------------------------------------------------------
Walking_Sound_Left: 
		lda Player_WalkIndex_L			// load the left walking index
		cmp #$01 						// if it is 1  (of total 12)
		beq Step1Sound 					// then play the 1st step sound
		cmp #$06 						// if it is 6  (of total 12)
		beq Step2Sound 					// then play the 2nd step sound
		rts 							// so for 12 frames, only 2 times is sufficient

		Walking_Sound_Right: 
		lda Player_WalkIndex_R
		cmp #$01
		beq Step1Sound
		cmp #$06
		beq Step2Sound
		rts 

CollectSound:
		lda #$0f 
		sta $d418
		lda #0 				// sfx number
		ldy #2 				// voice number
		jsr $c04a
		rts 

AntiCrateSound:
		lda #$0f 
		sta $d418
		lda #1
		ldy #2
		jsr $c04a
		rts 

Step1Sound: 
		lda #$0f 						
		sta $d418
		lda #2 						
		ldy #1 						
		jsr $c04a 					
		rts 

Step2Sound:
		lda #$0f 
		sta $d418
		lda #3
		ldy #1
		jsr $c04a
		rts 

DoorOpeningSound:
		lda #$0f 
		sta $d418
		lda #4
		ldy #2
		jsr $c04a
		rts 

RobotBMShhoting:
		lda #$0f 
		sta $d418
		lda #5
		ldy #2
		jsr $c04a
		rts 

RobotTShooting:
		lda #$0f 
		sta $d418
		lda #6
		ldy #2
		jsr $c04a
		rts 

CollectTimeSound: 
		lda #$0f 
		sta $d418
		lda #7 		
		ldy #2 		
		jsr $c04a
		rts 

RobotsMovingSound:
		lda #$08 
		sta $d418
		lda #8
		ldy #3
		jsr $c04a
		rts 

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
.label LEFT_SCREEN_EDGE = $0e
.label RIGHT_SCREEN_EDGE= $9e
.label BORDER_COLOR		= $d020
.label BACKGROUND_COLOR	= $d021
.label MULTICOLOR_1		= $d022
.label MULTICOLOR_2		= $d023
.label MULTICOLOR_3		= $d024
.label SPRITE_MULCOL_1	= $d025
.label SPRITE_MULCOL_2	= $d026
.label SPRITE_COLOR		= $d027
.label SCREEN_ADDRESS	= $4400
.label COLOR_RAM		= $d800
.label SPRITECOORD		= $d000
.label ROBOTBRIGHTEDGE  = $90
.label ROBOTBLEFTEDGE   = $0e 
.label ROBOTMRIGHTEDGE  = $33
.label ROBOTMLEFTEDGE   = $0e
.label ROBOTURIGHTEDGE  = $67
.label ROBOTULEFTEDGE   = $45
.label ROBOTTRIGHTEDGE  = $9e
.label ROBOTTLEFTEDGE   = $0e
.label TIMESTATUSADDR	= $47be
.label TIMESTATUSCOLOR	= $dbbe

Player_JumpIndex: 		.byte $00
Player_FallIndex:		.byte $00
Player_WalkIndex_R:		.byte $00
Player_WalkIndex_L:		.byte $00
Player_State: 			.byte $00
Player_WalkSpeed:		.byte $01
Collision_Temp:			.byte $00
JOY_ZP: 				.byte $00 
SpriteCharX:			.byte $00
SpriteCharY:			.byte $00
SpriteCharX_Coll:		.byte $00
SpriteCharY_Coll:		.byte $00
Space_Value:			.byte $00
Idle_Time:				.byte $00
Idle_Timeout: 			.byte $14
ScoreLo:				.byte $00
ScoreHi:				.byte $00
CrateNumber:			.byte $00
Door_Status:			.byte $00
Robot_B_State:			.byte $01 		// 0=face left 	1=face right
Robot_M_State:			.byte $01 		// 0=face left 	1=face right
Robot_U_State:			.byte $00 		// 0=face left 	1=face right
Robot_T_State:			.byte $00 		// 0=down 	1=up
Robot_T_Timer:			.byte $00
Robot_M_Timer:			.byte $00
Robot_U_Timer:			.byte $00
DoorOpen:				.byte $00
Time_Status:			.byte $00 		// holds the time left
Time_Status_Char:		.byte $4b
Time_Max:				.byte $30
Time_Counter:			.byte $80

Player_R_table:			.byte $80,$80,$80,$81,$81,$81,$82,$82,$82,$81,$81,$81
Player_L_table:			.byte $84,$84,$84,$85,$85,$85,$86,$86,$86,$85,$85,$85
PlayerJumpRSprite:		.byte $83
PlayerJumpLSprite:		.byte $87
PlayerStandRSprite:		.byte $81
PlayerStandLSprite:		.byte $85
Sprite_Collision:		.byte $00
Collision_Timer:		.byte $07
Collision_Flag:			.byte $00

JumpTable:		.byte $04, $04, $04, $03, $03, $03, $02, $02, $02, $02, $01, $01, $01, $01, $00

FallTable:		.byte $00, $01, $01, $01, $01, $02, $02, $02, $02, $03, $03, $03, $04, $04, $04
							.byte $04, $04, $04, $04, $04, $04, $04, $04
							.byte $04, $04, $04, $04, $04, $04, $04, $04
							.byte $04, $04, $04, $04, $04, $04, $04, $04

// sprite colors table
SpriteColors:	.byte $0c, $04, $04, $04, $0f, $0c, $0c, $05

// Initial sprite pointers
SpritePointers:	.byte $85, $89, $89, $88, $8a, $8c, $8c, $8d

// Initial sprite coordinates
SpriteCoordinates:	.byte 140,205,30,205,31,125,97,101,84,58,0,0,20,0,0,20

ScreenLSB:  	.fill 25, <[$4400 + i * $28]
ScreenMSB:		.fill 25, >[$4400 + i * $28]

// Sound effects file---------------------------------------------------------------------------

* = $4400 "map_data"
.import binary "Platform (1x1)_4 - (8bpc, 40x25) Map.bin"

* = $5000 "charset_data"
.import binary "Platform (1x1)_4 - Chars.bin"

* = $5800  "charset_attrib_L1_data"
.import binary "Platform (1x1)_4 - CharAttribs_L1.bin"

*=$6000 "Sprite data"
.import binary "Sprites.bin"

*=$c000 "Sound Effects"
.import c64 "Sounds2.prg"


