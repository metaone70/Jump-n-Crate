// Default-segment:
  // $0801-$080c Basic Program
  // $0810-$0e7f Program Listing
  // $1800-$1a11 Variables and tables
  // $2000-$222c PlayerControl
  // $222d-$2447 MoveRobots
  // $2448-$24ed CheckPlayerCollection
  // $5000-$57ff Character Set Data
  // $5800-$58ff Character Set Attrrib
  // $6000-$637f Sprite Data
  // $7000-$8387 Map Data
  // $c000-$c40c Sound Effects

.label SpritePos					= $0370 
.label SCREEN_LOOKUP 			= $f7 				// ($f7 + $f8)
.label SCREEN_LOOKUP2			= $f9 				// ($f9 + $fa)
.label CHARCOLORRAM				= $96 				// ($96 + $97)
.label TIME_LOOKUP				= $fb 				// ($fb + $fc)
.label TIME_COLOR_LOOKUP	= $fd 				// ($fd + $fe)
.label BelowCharVal  			= $0383
.label OnCharVal					= $0384
.label Sprite_Pointer			= $47f8
.eval var Player_x 				= SpritePos
.eval var Player_y 				= SpritePos+1
.var MapAddress						= $7000
.label SPBACKGR_COLL_REG 	= $d01f
.const TotalCrateNumber 	= 11

#import "helper.asm"
#import "PlayerControl.asm"
#import "MoveRobots_R1.asm"
#import "CheckPlayerCollection_R1.asm"

*=$0801 "Basic Program"
BasicUpstart($0810)

*=$0810 "Program Listing"
Start:
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

	   	jsr Initialize
			jsr SetupSprites 				// setup sprites

// game interrupts
INT:	sei 				// start interrupt routine
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
			jsr Check_Player_Exit
			jsr CheckPlayer_Collision
			jsr delay
			jsr Expand_Sprites
			jsr DecreaseTime
			jmp $ea31

// adjust MSB of sprites------------------------------------------------------
Expand_Sprites:	{
			lda #$f9								// wait for raster 249 to draw sprites
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
			sta PlayerCharX

	!:	lda Player_y				// get the x sprite coordinate
			sec 
			sbc #29 						// subtract #29 for the top border and sprite offset
			lsr  							  // divide by 8 -> 1 character is 8 bits long
			lsr 
			lsr 
			sta PlayerCharY		  // store it to character y variable
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
			lda PlayerCharY 				// get sprite y char value
			tay
			lda ScreenLSB,y 				// load the yth value from screen LSB table
			sta SCREEN_LOOKUP 				// and store it to the ZP address
			lda ScreenMSB,y 				// load the yth value from screen LSB table
			sta SCREEN_LOOKUP + 1 			// and store it to the ZP+1 address
			lda PlayerCharX
			tay 											// get sprite x char value 
			lda (SCREEN_LOOKUP),y 			// load the value at the ZP address by adding the x value
			sta BelowCharVal 				// store the character value to the variable
			rts
	}

AdjustSpriteYCoor: {					// to obtain pixel perfect landing
			lda PlayerCharY					// by doing the opposite of character calculation routine
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
			lda PlayerCharY 						// get sprite y char value
			tay
			dey  												// we decrease it by 1, so the sprite char we are after is just above the floor line
			lda ScreenLSB,y 						// load the yth value from screen LSB table
			sta SCREEN_LOOKUP2 					// and store it to the ZP address
			lda ScreenMSB,y 						// load the yth value from screen LSB table
			sta SCREEN_LOOKUP2 + 1 			// and store it to the ZP+1 address

			lda PlayerCharX
			tay 												// get sprite x char value 
			lda (SCREEN_LOOKUP2),y 			// load the value at the ZP address by adding the x value
			sta OnCharVal 							// store the character value to the variable
			rts
	}

// Check player vs robots/bullets (sprite) collisions
CheckPlayer_Collision: {
			lda $d01e
			cmp #%00000011
			beq !RobotCollided+
			cmp #%00000101
			beq !RobotCollided+
			cmp #%00001001
			beq !RobotCollided+
			cmp #%00010001
			beq !RobotCollided+
			cmp #%00100001		
			beq !BulletCollided+
			cmp #%01000001		
			beq !BulletCollided+
			cmp #%10000001		
			beq !BulletCollided+
			jmp !Exit+


// If player-robots collided (there is a timer here)
!RobotCollided:
			lda Collision_Flag			// check if there has been a collision
			cmp #$01 								// if yes, exit (there is a timeout)
			beq !Exit+

															// in order to prevent full degredationf of time due to collision
															// we are checking for a short counter time of 7 cycles		
!:		lda Collision_Timer			// check the collision timer if it is 7 (initial value)
			cmp #$05 								// if it is less than 7, then there is no need to decrease time status
			bne !+						
			jsr Decrease_Time_Status	// if it is 7, then first decrease the time status and then
			jsr PlayerShot
!:		dec Collision_Timer			// decrease the collision timer
			lda Collision_Timer	
			cmp #$00 								// did the timer reached zero?
			bne !Exit+							// if not, exit
			lda #$05 								// if it did, then reset the timer to 7
			sta Collision_Timer
			lda #$00 								// and also reset the collision flag
			sta Collision_Flag
			jmp !Exit+

!BulletCollided:
			jsr Decrease_Time_Status
			jsr PlayerShot
!Exit:	
			rts 
}

// Decrease time---------------------------------------------------------------
Decrease_Time_Status: {
			dec Time_Status 			// decrease the time status
			lda Time_Status 			// check status
			bne !+ 								// if not zero, skip next check
			jmp GameOver 					// if zero, go to Game Over section

									// $47 = empty char
									// $48 = 1/4 full char	
									// $49 = 1/2 full char	
									// $4a = 3/4 full char
									// $4b = full char	
!:		dec Time_Status_Char 		// decrease the bar by changing the character
			lda Time_Status_Char		// check the current status character
			cmp #$4d 					// if it is not empty, then continue printing
			bcs !print+
										// here, the status char is empty
			lda #$51 					// so load the full char to status char
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

!Exit:	
			rts 
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
			lda #$51 						// full health character
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
			lda #$51 				// full health character
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

!Exit:	
			rts 
}

// check if the door is open and the player exits--------------------------------------------
Check_Player_Exit: {
			lda DoorOpenFlag			// check if the door is open
			beq !Exit+						// if not, exit
			lda PlayerCharY				// check if the player is on the same level as the door
			cmp DoorYValue 			
			bne !Exit+ 						// if not, exit
			lda PlayerCharX
			cmp DoorXValue
			bne !Exit+

			jmp AdvanceLevel

!Exit:	
			rts 
}
 
// Game Over ------------------------------------------------------------------------------
GameOver: {
			lda #%00000001 
			sta $d015 				// turn off all sprites 
			PrintScreen(GameOverText1,$4400,17,13,1,9)
			PrintScreen(GameOverText2,$4400,10,14,1,23)
			WaitSpaceKey()
			jmp Start 
}

// Increment score-----------------------------------------------------------
AddScore: {
			inc $47aa
			lda $47aa
			cmp #$2b
			bne !+
			lda #$21
			sta $47aa
			inc $47a9
			lda $47a9
			cmp #$2b
			bne !+
			lda #$21
			sta $47a9
			inc $47a8
!:		rts 
}

// After collecting all crates, the door will open--------------------------------------
OpenDoor: {
			lda CurrentLevel
			cmp #$01
			beq !Level1+
			cmp #$02
			beq !Level2+			
			cmp #$03
			beq !Level3+			
			cmp #$04
			beq !Level4+			
			cmp #$05
			beq !Level5+
			rts 

!Level1:
			PrintChar2Screen(0,$4400,36,20)
			PrintChar2Screen(0,$4400,36,21)
			jsr DoorOpeningSound
			rts

!Level2:
			PrintChar2Screen(0,$4400,3,20)
			PrintChar2Screen(0,$4400,3,21)
			jsr DoorOpeningSound
			rts

!Level3:
			PrintChar2Screen(0,$4400,35,04)
			PrintChar2Screen(0,$4400,35,05)
			jsr DoorOpeningSound
			rts

!Level4:
			PrintChar2Screen(0,$4400,36,20)
			PrintChar2Screen(0,$4400,36,21)
			jsr DoorOpeningSound
			rts

!Level5:
			PrintChar2Screen(0,$4400,36,20)
			PrintChar2Screen(0,$4400,36,21)
			jsr DoorOpeningSound
			rts
}

// Decrease time as time goes by--------------------------------------------------------
DecreaseTime: {

			dec Time_Counter						// decrement he time counter
			bne !Exit+ 									// check if it is if not zero, then exit
			jsr Decrease_Time_Status 		// if zero, then counter = 0, so decrease time status
			lda #$60 										// start the countdown again
			sta Time_Counter

!Exit:	rts 
}

//Playing sound effects------------------------------------------------------------
Walking_Sound_Left: 
			lda Player_WalkIndex_L			// load the left walking index
			cmp #$01 										// if it is 1  (of total 12)
			beq Step1Sound 							// then play the 1st step sound
			cmp #$06 										// if it is 6  (of total 12)
			beq Step2Sound 							// then play the 2nd step sound
			rts 												// so for 12 frames, only 2 times is sufficient

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

RobotMUShhoting:
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

PlayerShot:
			lda #$0f 
			sta $d418
			lda #8 		
			ldy #2 		
			jsr $c04a
			rts 

RobotsMovingSound:
			lda #$08 
			sta $d418
			lda #9
			ldy #3
			jsr $c04a
			rts 

// Initialize game variables------------------------------------------------------
Initialize: {

			lda #<SpritePointers
			sta $a4 
			lda #>SpritePointers
			sta $a5
			lda #<SpriteCoordinates
			sta $a6
			lda #>SpriteCoordinates
			sta $a7
			lda #<DoorLocation
			sta $a8 
			lda #>DoorLocation
			sta $a9 
			lda #<RobotEdgeCoordinates
			sta $aa 
			lda #>RobotEdgeCoordinates
			sta $ab 
			lda #<ScreenBGTable
			sta $ac 
			lda #>ScreenBGTable
			sta $ad 

		  lda Time_Max				// set the time to max at the beginning
		  sta Time_Status

			lda #[STATE_FACE_LEFT + STATE_WALK_LEFT]	// set initial direction as facing right
			sta Player_State        

			lda Idle_Timeout
			sta Idle_Time

			lda #$00 					// door closed at the beginning
			sta DoorOpenFlag
			lda #$00  					// initialize the crates collected
			sta CrateNumber

			lda Time_Max
			sta Time_Status
			lda #$51
			sta Time_Status_Char

			lda #$01
			sta CurrentLevel

			lda #<TIMESTATUSADDR		// load the time status address to ZP
			sta TIME_LOOKUP
			lda #>TIMESTATUSADDR
			sta TIME_LOOKUP+1
			lda #<TIMESTATUSCOLOR		// load time status color to ZP
			sta TIME_COLOR_LOOKUP
			lda #>TIMESTATUSCOLOR
			sta TIME_COLOR_LOOKUP+1	

			ldy #$00
			lda ($a8),y 
			sta DoorXValue
			iny 
			lda ($a8),y 
			sta DoorYValue

			ldy #$00
!:		lda ($aa),y 
			sta ROBOTBRIGHTEDGE,y 
			iny 
			cpy #$08
			bne !-

			ldy #$00
			lda ($ac),y 
			sta ScreenBGChar

			ldx #$01
			lda RobotM_Bullet_Table,x 
			sta RobotMBullet_Trigger
			lda RobotU_Bullet_Table,x 
			sta RobotUBullet_Trigger

		  DrawScreen2($7000,$4400)

			rts 
}

//Setup sprites------------------------------------------------------------------------------
SetupSprites: {
			lda #$ff         						// set multicolor bit
			sta $d01c										// for sprites 0-7

			lda #BLACK              		// multicolor register
			sta SPRITE_MULCOL_1    			// black 
			lda #WHITE									// and white
			sta SPRITE_MULCOL_2

			ldy #$00 								
	!:	lda ($a6),y 
			sta SpritePos,y 
			iny 
			cpy #$0f 										// do it for 8 sprites
			bne !-

			ldy #$00
	!:	lda ($a4),y  								// set sprite pointer --> VIC BANK + $2000 (128 x 64)
			sta Sprite_Pointer,y    
			lda SpriteColors,y  				// load sprite colors from table
			sta SPRITE_COLOR,y  				// store it to color registers
			iny 
			cpy #$08
			bne !-

			lda #%11111111
			sta $d015               	// enable all sprites

			rts 
	}

// Change the level parameters-------------------------------------------------------
AdvanceLevel: {

			inc CurrentLevel 
			lda CurrentLevel
			cmp #$06
			bcc !+
			jmp GameOver

!:		lda #%00001111 
			sta $d015 									// turn off bullet sprites
			PrintScreen(AdvanceLevelText1,$4400,7,13,1,26)
			PrintScreen(AdvanceLevelText2,$4400,7,14,1,26)
			PrintScreen(AdvanceLevelText3,$4400,7,15,1,26)

!Joy:	lda $dc00
			lsr 
			lsr 
			lsr 
			lsr 
			lsr 
			bcc !+
			jmp !Joy-

!:		inc $479f										// increment level on screen

			ldx #$00
!Sc:	lda $4798,x 
			sta StatusRow,x 			
			inx 
			cpx #$28
			bne !Sc-

//Draw screen according to the level
			lda CurrentLevel 
			cmp #$02
			bne !+ 
			jmp !Level2Screen+
!:		cmp #$03
			bne !+ 
			jmp !Level3Screen+
!:		cmp #$04
			bne !+
			jmp !Level4Screen+			
!:		jmp !Level5Screen+

!Level2Screen:
			DrawScreen2($73e8,$4400)
			jmp !+
!Level3Screen:
			DrawScreen2($77d0,$4400)
			jmp !+
!Level4Screen:
			DrawScreen2($7bb8,$4400)
			jmp !+
!Level5Screen:
			DrawScreen2($7fa0,$4400)

!:
			ldx #$00
!Sc:	lda StatusRow,x 
			sta $4798,x 			
			inx 
			cpx #$28
			bne !Sc-

			ldy #$00
!Cl:	lda $4798,y 		// get the character value from screen position
			tax 
   		lda $5800,x 			// get the attribute of the character
   		sta $db98,y 
   		iny
   		cpy #$28
   		bne !Cl-

			ldx #$00 											// increment sprite pointers
!:		inc $a4 
			inx 
			cpx #$08
			bne !-

			ldy #$00
	!:	lda ($a4),y  								// load sprite porinters
			sta Sprite_Pointer,y    
			iny 
			cpy #$08
			bne !-

			ldx #$00 											// increment sprite positions table pointer
!:		inc $a6 
			inx 
			cpx #$10
			bne !-

			ldy #$00 								
	!:	lda ($a6),y  								// load sprite starting positions
			sta SpritePos,y 
			iny 
			cpy #$10 									
			bne !-

			ldx #$00 										// increment robot limits table pointer
!:		inc $aa 
			inx 
			cpx #$08
			bne !-

			ldy #$01
!:		lda ($aa),y 
			sta ROBOTBRIGHTEDGE,y 
			iny 
			cpy #$08
			bne !-

			inc $a8
			inc $a8 
			ldy #$00
			lda ($a8),y 
			sta DoorXValue
			iny 
			lda ($a8),y 
			sta DoorYValue

			inc $ac 
			ldy #$00
			lda ($ac),y 
			sta ScreenBGChar

			lda #[STATE_FACE_RIGHT + STATE_WALK_RIGHT]	
			sta Player_State        

			lda Idle_Timeout
			sta Idle_Time

			lda #$00 					// door closed at the beginning
			sta DoorOpenFlag
			lda #$00  					// initialize the crates collected
			sta CrateNumber

			lda CurrentLevel
			tax 
			lda RobotM_Bullet_Table,x 
			sta RobotMBullet_Trigger
			lda RobotU_Bullet_Table,x 
			sta RobotUBullet_Trigger

			lda #$ff 
			sta $d015 
			jmp INT  

}

//variables----------------------------------------------------
* = $1800 "Variables and tables"
.label STATE_JUMP 			= %00000001  // 1
.label STATE_FALL 			= %00000010  // 2 			
.label STATE_WALK_LEFT  = %00000100  // 4  	// walk left = 4 + 16 =20    % 0001 0100   $14
.label STATE_WALK_RIGHT = %00001000  // 8 	// walk right = 8 + 32 = 40  % 0010 1000   $28
.label STATE_FACE_LEFT  = %00010000  //16 
.label STATE_FACE_RIGHT = %00100000  //32
.label JOY_UP 					= %00000001
.label JOY_DN 					= %00000010
.label JOY_LT 					= %00000100
.label JOY_RT 					= %00001000
.label JOY_FR 					= %00010000
.label JOY_PORT_2 			= $dc00
.label LEFT_SCREEN_EDGE = $0e
.label RIGHT_SCREEN_EDGE= $9e
.label BORDER_COLOR			= $d020
.label BACKGROUND_COLOR	= $d021
.label MULTICOLOR_1			= $d022
.label MULTICOLOR_2			= $d023
.label MULTICOLOR_3			= $d024
.label SPRITE_MULCOL_1	= $d025
.label SPRITE_MULCOL_2	= $d026
.label SPRITE_COLOR			= $d027
.label SCREEN_ADDRESS		= $4400
.label COLOR_RAM				= $d800
.label SPRITECOORD			= $d000
.label TIMESTATUSADDR		= $47be
.label TIMESTATUSCOLOR	= $dbbe

ROBOTBRIGHTEDGE:		  .byte $00
ROBOTBLEFTEDGE:				.byte $00
ROBOTMRIGHTEDGE:			.byte $00
ROBOTMLEFTEDGE:				.byte $00
ROBOTURIGHTEDGE:			.byte $00
ROBOTULEFTEDGE:				.byte $00
ROBOTTRIGHTEDGE:			.byte $00
ROBOTTLEFTEDGE:				.byte $00

CurrentLevel:					.byte $00
Player_JumpIndex: 		.byte $00
Player_FallIndex:			.byte $00
Player_WalkIndex_R:		.byte $00
Player_WalkIndex_L:		.byte $00
Player_State: 				.byte $00
Player_WalkSpeed:			.byte $01
Collision_Temp:				.byte $00
JOY_ZP: 							.byte $00 
PlayerCharX:					.byte $00
PlayerCharY:					.byte $00
PlayerCharX_Coll:			.byte $00
PlayerCharY_Coll:			.byte $00
CrateCharValue:				.byte $43
TimeCapsuleCharValue:	.byte $44 
AntiCrateCharValue:		.byte $4c  
Idle_Time:						.byte $00
Idle_Timeout: 				.byte $14
CrateNumber:					.byte $00
Door_Status:					.byte $00
Robot_B_State:				.byte $01 		// 0=face left 	1=face right
Robot_M_State:				.byte $01 		// 0=face left 	1=face right
Robot_U_State:				.byte $00 		// 0=face left 	1=face right
Robot_T_State:				.byte $00 		// 0=down 	1=up
Robot_T_Timer:				.byte $00
Robot_M_Timer:				.byte $00
Robot_U_Timer:				.byte $00
Robot_T_Bullet_Flag:	.byte $00
Robot_M_Bullet_Flag:	.byte $00
Robot_U_Bullet_Flag:	.byte $00
Robot_M_Delay_Flag:		.byte $00
Robot_U_Delay_Flag:		.byte $00
RobotMBullet_Trigger:	.byte $00
RobotUBullet_Trigger:	.byte $00

DoorOpenFlag:					.byte $00
DoorXValue:						.byte $00
DoorYValue:						.byte $00
Time_Status:					.byte $00 		// holds the time left
Time_Status_Char:			.byte $51
Time_Max:							.byte $3c
Time_Counter:					.byte $60

Player_R_table:				.byte $80,$80,$80,$81,$81,$81,$82,$82,$82,$81,$81,$81
Player_L_table:				.byte $84,$84,$84,$85,$85,$85,$86,$86,$86,$85,$85,$85
PlayerJumpRSprite:		.byte $83
PlayerJumpLSprite:		.byte $87
PlayerStandRSprite:		.byte $81
PlayerStandLSprite:		.byte $85
Sprite_Collision:			.byte $00
Collision_Timer:			.byte $07
Collision_Flag:				.byte $00
ScreenBGChar:					.byte $00

ScreenBGTable:				.byte $3e,$52,$58,$00,$00

JumpTable:		.byte $04, $04, $04, $03, $03, $03, $02, $02, $02, $01, $01, $01, $01, $01, $00

FallTable:		.byte $00, $01, $01, $01, $01, $01, $02, $02, $02, $03, $03, $03, $04, $04, $04
							.byte $04, $04, $04, $04, $04, $04, $04, $04
							.byte $04, $04, $04, $04, $04, $04, $04, $04
							.byte $04, $04, $04, $04, $04, $04, $04, $04

// sprite colors table
SpriteColors:	.byte $0c, $04, $04, $04, $0f, $0c, $0c, $05

// Sprite pointers
SpritePointers:		.byte $85, $89, $89, $88, $8a, $8c, $8c, $8d 			// level 1
									.byte $85, $89, $89, $88, $8a, $8c, $8c, $8d 			// level 2
									.byte $85, $89, $89, $88, $8a, $8c, $8c, $8d 			// level 3
									.byte $85, $89, $89, $88, $8a, $8c, $8c, $8d 			// level 4
									.byte $85, $89, $89, $88, $8a, $8c, $8c, $8d 			// level 5

// Starting sprite coordinates
//												Player  RobotB  RobotM  RobotU  RobotT  
SpriteCoordinates:	.byte $8c,$cd,$1e,$cd,$1f,$7d,$61,$65,$54,$37,$00,$00,$00,$00,$00,$00 			// level 1
										.byte $16,$cd,$8c,$cd,$2d,$5d,$63,$75,$54,$37,$00,$00,$00,$00,$00,$00 			// level 2
										.byte $8c,$cd,$1e,$cd,$1d,$55,$90,$7d,$54,$37,$00,$00,$00,$00,$00,$00 			// level 3
										.byte $00,$00,$00,$00,$00,$00,$00,$00,$54,$37,$00,$00,$00,$00,$00,$00 			// level 4
										.byte $00,$00,$00,$00,$00,$00,$00,$00,$54,$37,$00,$00,$00,$00,$00,$00 			// level 5

// Door locations (x/y values)
DoorLocation:				.byte $24,$16 		// level 1
										.byte $03,$16			// level 2
										.byte $23,$06			// level 3
										.byte $24,$16			// level 4
										.byte $24,$16			// level 5

// 													RBR,RBL,RMR,RML,RUR,RUL,RTR,RTL
RobotEdgeCoordinates:	.byte $90,$0e,$33,$0e,$67,$45,$9c,$10 		// level 1
											.byte $9c,$14,$3a,$1b,$76,$4d,$9c,$10 		// level 2  RobotM üstte, RobotU ortada
											.byte $90,$0e,$2d,$0e,$9e,$7c,$9c,$10 		// level 3
											.byte $90,$0e,$33,$0e,$67,$45,$9c,$10 		// level 4
											.byte $90,$0e,$33,$0e,$67,$45,$9c,$10 		// level 5

RobotM_Bullet_Table:	.byte $00,$32,$20,$13,$00,$00 						// load to RobotUBullet_Trigger
RobotU_Bullet_Table:	.byte $00,$60,$70,$90,$00,$00

GameOverText1:	.text "game over" 
GameOverText2:	.text "hit space to start over"

AdvanceLevelText1:	.text "congratulations. you have "
AdvanceLevelText2:	.text "advanced to the next level"
AdvanceLevelText3:	.text "  press fire to continue  "

ScreenLSB:  	.fill 25, <[$4400 + i * $28]
ScreenMSB:		.fill 25, >[$4400 + i * $28]

StatusRow:		.fill 40, 0 

// Sound effects file---------------------------------------------------------------------------

* = $5000 "Character Set Data"
.import binary "Platform (1x1)_6 - Chars.bin"

* = $5800  "Character Set Attrrib"
.import binary "Platform (1x1)_6 - CharAttribs_L1.bin"

*=$6000 "Sprite Data"
.import binary "Sprites_R1.bin"

* = $7000 "Map Data"
.import binary "Platform (1x1)_6 - (8bpc, 40x125) Map.bin"

*=$c000 "Sound Effects"
.import c64 "Sounds3.prg"


