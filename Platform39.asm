// Jump'n Crate (C) metesev 2023
// Start date: 13.03.2023
// End date: 10.04.2023
// Default-segment:
  // $0801-$080c Basic Program
  // $0810-$13c1 Program Listing
  // $1800-$1a9d Variables and tables
  // $2000-$2226 PlayerControl
  // $2227-$2441 MoveRobots
  // $2442-$24f0 CheckPlayerCollection
  // $4800-$4b7f Sprite Data
  // $5000-$57ff Character Set Data
  // $5800-$58ff Character Set Attrib
  // $6000-$8710 Bitmap screen
  // $9000-$93dc External sound driver
  // $a000-$b387 Map Data
  // $b400-$b7e7 Info screen
  // $b800-$bbe7 Final (success) screen
  // $bc00-$bfe7 Final (fail) screen
  // $c000-$cacf Song and jingles

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
.var MapAddress						= $a000
.label SPBACKGR_COLL_REG 	= $d01f
.const TotalCrateNumber 	= 11
.eval var RobotB_x		= SpritePos+2
.eval var RobotB_y		= SpritePos+3
.eval var RobotM_x		= SpritePos+4
.eval var RobotM_y		= SpritePos+5
.eval var RobotU_x		= SpritePos+6
.eval var RobotU_y		= SpritePos+7
.eval var RobotT_x		= SpritePos+8
.eval var RobotT_y		= SpritePos+9
.eval var RobotMBul_x 	= SpritePos+10
.eval var RobotMBul_y 	= SpritePos+11
.eval var RobotUBul_x 	= SpritePos+12
.eval var RobotUBul_y 	= SpritePos+13
.eval var RobotTBul_x 	= SpritePos+14
.eval var RobotTBul_y 	= SpritePos+15

#import "helper.asm"
#import "PlayerControl.asm"
#import "MoveRobots_R3.asm"
#import "CheckPlayerCollection_R1.asm"

*=$0801 "Basic Program"
BasicUpstart($0810)

*=$0810 "Program Listing"

		sei 
		lda #$36			// disable BASIC ROM
	   	sta $01 			// in order to use more ZP

      	lda #$a7 			// when hit runstop/restore
      	sta $0318 		// nothing happens... it points to the  
      	lda #$08    	// INT section
      	sta $0319

		SetBorderColor(BLACK)
		SetBackgroundColor(BLACK)

// prepare screen for bitmap graphics
	    lda #$3b		           // %0011 1011 enable bitmap, show screen and 25 rows
    	sta $d011
    	lda #$18 							// %0001 1000  enable multi-color bitmap mode and 40 cols 
    	sta $d016
     	lda #$78      				// %0111 1000  video matrix and text relative addresses
     	sta $d018        
    	lda #$16       				// %0000 0110 select VIC bank 1
    	sta $dd00

    	lda #$00
     	ldx #$c0 
    	ldy #$00
    	jsr $9000

      	jsr IntroSong
		DrawBitmap($7f40,$5c00,$8328)
 		WaitSpaceKey()

	  	lda #%00011011   		// %0001 1000   Bit #3: 1 = 25 rows,  Bit #4: 1 = Screen on   
	  	sta $d011
	   	lda #%00011000 			// %0001 1000 Bit #3:1 = 40 columns,  Bit #4: 1 = Multi-color mode on
	   	sta $d016                
	   	lda #%00010100   		// %0001 0100 bit 1-3=text character address 	%010, 2: $1000-$17FF ($4000+$1000=$5000)
	   	sta $d018       	  // Values %010 and %011 in VIC bank #0 and #2 select Character ROM instead.
													// 		 bit 4-7=video matrix base address  %0001, %0001, 1: $0400-$07FF ($4400)
	   	lda #%00010110      // %0001 0111 bits 0&1=select 16K bank for VICII %10, 2: Bank #1, $4000-$7FFF
	   	sta $dd00 

	   	lda #LIGHT_GREY
	   	sta MULTICOLOR_1
	   	lda #WHITE			
	   	sta MULTICOLOR_2
Start:
		jsr InfoScreen
		jsr Initialize
		jsr SetupSprites 				// setup sprites
		jsr MuteSound

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

		ldx #$c0 			// initialize the sound driver
    	ldy #$03
    	jsr $9003

		cli
		jmp *	

// main loop of the animation---------------------------------------------
mloop: 	
		inc $d019					// acknowledge VIC IRQ
		jsr $9006					// call audio driver
		jsr MoveRobots
		jsr Player_Walk
		jsr CalcSpriteChar
		jsr JumpAndFall
		jsr Check_Player_Collection
		jsr Check_Player_Exit
		jsr CheckPlayer_Collision
		jsr CheckPlayer_Collision_Robots
		jsr CheckPlayer_Collision_Bullets
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
		waitForRasterLine($32)
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

// Check sprite hardware collision register and store it to a temp variable
CheckPlayer_Collision: {
		// lda #$00
		// sta Collision_Temp
		lda $d01e
		sta Collision_Temp
		rts
}

// Check player vs robots (sprite) collisions------------------------------------------
CheckPlayer_Collision_Robots: {
		lda Collision_Flag				// check if there has been a collision with robots	
		beq !++									  // if collision flag is not set, go check collision
		lda Collision_Timer 			// otherwise check the timer
		bne !+ 										// if not zero, exit
		lda #$10
		sta Collision_Timer
		lda #$00
		sta Collision_Flag
		jmp !Exit+

!:		dec Collision_Timer 			
		jmp !Exit+

!:		lda Collision_Temp 			// if collision flag is not set; check for collision
		and #%00011111
		cmp #%00000011					// $3
		beq !RobotCollided+
		cmp #%00000101 					// $5
		beq !RobotCollided+ 
		cmp #%00001001					// $9
		beq !RobotCollided+
		cmp #%00010001 					// $11
		beq !RobotCollided+

!Exit:
		rts
}		

// Check player vs bullets (sprite) collisions----------------------------------------
CheckPlayer_Collision_Bullets: {
		lda Collision_Temp
		and #%11100001
		cmp #%00100001					// $21
		beq BulletMCollided
		cmp #%01000001					// $41
		beq BulletUCollided
		cmp #%10000001					// $81
		beq BulletTCollided
		lda #$00
		sta Collision_Temp
		rts
}

// there has been collision with robots
!RobotCollided: {
		jsr Decrease_Time_Status
		jsr PlayerShot
		lda #$10
		sta Collision_Timer
		lda #$01
		sta Collision_Flag
		lda #$00
		sta Collision_Temp
		rts
}

// there has been collision with bullets
BulletMCollided: {
		jsr Decrease_Time_Status		// decrease time status
		jsr PlayerShot 							// play the sound
		lda #$00 										// reset the bullet on flag
		sta Robot_M_Bullet_Flag 		// and the bullet coordinates
		sta RobotMBul_x
		sta RobotMBul_y
		rts
} 

BulletUCollided: {
		jsr Decrease_Time_Status
		jsr PlayerShot
		lda #$00
		sta Robot_U_Bullet_Flag
		sta RobotUBul_x
		sta RobotUBul_y
		rts 
}

BulletTCollided: {
		jsr Decrease_Time_Status
		jsr PlayerShot
		lda #$00
		sta Robot_T_Bullet_Flag
		sta RobotTBul_x
		sta RobotTBul_y
		rts 
}

// check robot timer outside of the collision detection routine-----------------------------
Check_Robot_Collision_Timer:	{	
		lda Collision_Flag			// check if there has been a collision		
		beq !Exit+ 							// if no collision, exit
		dec Collision_Timer			// if collision happened, decrement the collision timer
		lda Collision_Timer	
		cmp #$00 								// did the timer reached zero?
		bne !Exit+							// if not, exit
		lda #$00 								// if yes, reset the collision flag
		sta Collision_Flag

!Exit:
}

// Decrease time---------------------------------------------------------------
Decrease_Time_Status: {
		dec Time_Status 			// decrease the time status
		lda Time_Status 			// check status
		bne !+ 								// if not zero, skip next check
		jmp GameOver_Bad 			// if zero, go to Game Over failed section

// $47 = empty char
// $48 = 1/4 full char	
// $49 = 1/2 full char	
// $4a = 3/4 full char
// $4b = full char	
!:		dec Time_Status_Char 		// decrease the bar by changing the character
		lda Time_Status_Char		// check the current status character
		cmp #$4d 								// if it is not empty, then continue printing
		bcs !print+
													// here, the status char is empty
		lda #$50 								// so load the full char to status char
		sta Time_Status_Char
		dec $fb  								// decrement the LSB of status char, so it is one left char
		dec $fd 								// also decrement the color address LSB
		lda $fb 
		cmp #$b3 								// also check if we are at the last time status box
		bcs !print+ 						// if no, pass on to printing
		jmp GameOver_Bad 				// if yes, then the boxes are all empty, so finish the game 

!print:
		ldy #$00 								// load the status character
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
		cmp #Time_Max 				// if it is already maximized
		bcc !+  							// if not, move on and decrease it
		jmp !Exit+

!:		lda $fb 							// load the TIME_LOOKUP ZP address lomem
		cmp #$be  						// check if the pointer points to the last (rightmost) character
		bcc !+ 								//if it is smaller than $be, then we have decreased the time bar before
		lda #$51
		sta Time_Status_Char
		ldy #$00 							// load the full health character
		lda Time_Status_Char	// to the last bar
		sta (TIME_LOOKUP),y   
		ldx Time_Status_Char 	// and then get the character attrib and store it to the color RAM
		lda $5800,x 
		sta (TIME_COLOR_LOOKUP),y 
		lda Time_Max
		sta Time_Status
		jmp !Exit+

!:		inc Time_Status 			// we are here so we have more than one bar decreased
		inc Time_Status 			// so increase it 4 times (a bar=character)
		inc Time_Status
		inc Time_Status
		lda Time_Status
		cmp #Time_Max
		bcc !+
		lda Time_Max
		sta Time_Status

!:		ldy #$00 							// load the full health character
		lda #$51 							// to the pointer character
		sta (TIME_LOOKUP),y 
		ldx #$51
		lda $5800,x 
		sta (TIME_COLOR_LOOKUP),y 
		inc $fb  							// increment the pointer address towards right (lomem)
		inc $fd  							// and also increment its color address (lomem)
		ldy #$00
		lda Time_Status_Char  // put the current character to that address, 
		sta (TIME_LOOKUP),y 	// so it moves 1 character right
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
 
// Game Over with success----------------------------------------------------------------------
GameOver_Good: {
		lda #%00001111 
		sta $d015 									// turn off bullet sprites 
		ldx #$00 										// store the final score to temp location
!:		lda $47a8,x 
		sta Score_temp,x 
		inx 
		cpx #$04
		bne !-
		ldx #$00 										// get the time bar characters
!Sc:	lda $47b3,x  								// and store it to a table
		sta StatusRow,x 			
		inx 
		cpx #$0c
		bne !Sc-
		jsr GameOverSong
		PrintScreen(GameOverGoodText1,$4400,12,14,1,16)
		PrintScreen(GameOverGoodText2,$4400,5,15,1,29)
	    ldx #$00     
!del:   lda #$00
        sta DelayTemp
        sta DelayTemp+1
!wait1: inc DelayTemp+1
!wait2: inc DelayTemp
        lda DelayTemp
        cmp #$ff
        bne !wait2-
        lda DelayTemp+1
        cmp #$ff                
        bne !wait1-
        inx 
        cpx #$08
        bne !del-
		ClearScreen($4400,0)
		DrawScreen2($b800,$4400)
		ldx #$00 										// store the final score to temp location
!:		lda Score_temp,x 
		sta $46e2,x 
		inx 
		cpx #$04
		bne !-
		ldx #$00 										// get the time bar characters
!Sc:	lda StatusRow,x  						// and store it to a table
		sta $463e,x 			
		inx 
		cpx #$0c 
		bne !Sc-
		ldy #$00 									// and the characters' color info to their position
!Cl:	lda $463e,y 		  				// get the character value from screen position
		tax
   		lda $5800,x 							// get the attribute of the character
   		sta $da3e,y 
   		iny
   		cpy #$0c
   		bne !Cl-
   		lda #$3e 								// place the status row start address to ZP
   		sta $50
   		lda #$46
   		sta $51
   		ldy #$00 								// we need to find the character other than full
!: 		lda ($50),y 						// so we are checking against $51
   		cmp #$51
   		bne !+
   		iny 
   		cpy #$0c 
   		bne !-
!: 		sty TimeRow_Temp 				// we found the yth character, so we store that number it to temp variable
   		ldy TimeRow_Temp
   		ldx Time_Status 					// get the remaining time
!scr:	StoreState()
		waitForRasterLine($f0)
		waitForRasterLine($f0)
		RestoreState()
		inc $46e4 								// and add 10 for each time remaining 
		lda $46e4
		cmp #$2b 									// if it is >9
		bcc !row+
		lda #$21  								// we load 0 to 10s
		sta $46e4
		inc $46e3 								// and then we increment 100s
		lda $46e3
		cmp #$2b 									// is 100s is >9
		bcc!row+
		lda #$21 									// then we load 9 to 100s
		sta $46e3
		inc $46e2			 						// and increment 1000s

!row:	lda ($50),y 					// load the value of $463e + y (current bar)
		sta TimeChar_Temp			// and store that character to temp
		dec TimeChar_Temp			// we decrement that char value			
		lda TimeChar_Temp
		cmp #$4c   						// if it is < $4d (empty bar)
		bne !++ 							// then go and load 1-less-than-full bar
!:		dey 									// otherwise decrement the character address
		lda #$50 							// load the 1-less-than-full bar character
!:		sta ($50),y 					// store it to the current time bar address
		dex 									// decrement x (time value)
		bne !scr- 						// repeat it until it is 0

!Joy2:	
		lda $dc00
		lsr 
		lsr 
		lsr 
		lsr 
		lsr 
		bcc !+
!dl:	lda $d012
		bne !dl-
		jmp !Joy2-

!:		
		jsr MuteSound
		jmp Start 
}

// Game Over with fail------------------------------------------------------------------------
GameOver_Bad: {
		lda #%00001111 
		sta $d015 						// turn off bullet sprites 
		ldy #$4d 							// the game is over, the time status is zero 
		sty $47b3 						// but the last character on the low left is not empty
		lda $5800,y 					// so put an empty bar to the last character
		sta $dbb3  						// so the row looks empty
		ldx #$00 							// store the final score to temp location
!:		lda $47a8,x 
		sta Score_temp,x 
		inx 
		cpx #$04
		bne !-
		jsr GameOverSong
		PrintScreen(GameOverBadText,$4400,9,13,1,22) 
       	ldx #$00     
 !del:  lda #$00
        sta DelayTemp
        sta DelayTemp+1
!wait1: inc DelayTemp+1
!wait2: inc DelayTemp
        lda DelayTemp
        cmp #$ff
        bne !wait2-
        lda DelayTemp+1
        cmp #$ff                
        bne !wait1-
        inx 
        cpx #$08
        bne !del-
		ClearScreen($4400,0)
	  	DrawScreen2($bc00,$4400)
		ldx #$00 									// write the final score to screen
!:		lda Score_temp,x 
		sta $46ba,x 
		inx 
		cpx #$04
		bne !-

!Joy2:	
		lda $dc00
		lsr 
		lsr 
		lsr 
		lsr 
		lsr 
		bcc !+
!dl:	lda $d012
		bne !dl-
		jmp !Joy2-

!:		
		jsr MuteSound
		jmp Start 

}

// Increment score-----------------------------------------------------------
AddScore: {
		inc $47aa 					// increment 10s
		lda $47aa 					// is it greater than 9?
		cmp #$2b
		bne !+ 							
		lda #$21 						// if not, write 0 
		sta $47aa
		inc $47a9 					// and then increment 100s
		lda $47a9 					// is it greater than 9? 					
		cmp #$2b
		bne !+ 							// if not ,exit
		lda #$21 						// otherwise write 0 
		sta $47a9
		inc $47a8						// and increment 1000s (which will never happen)
!:		rts 
}

// Decrement score-----------------------------------------------------------
DecreaseScore: {
		lda $47ab 			// get the rightmost character
		cmp #$21  		  // if it is not zero (it is 5), branch below
		bne !++
		lda $47aa				// check the 10 digit
		cmp #$21
		bne!+						// it if it not zero (greater than zero), then proceed and make 5
		lda $47a9 			// if it is zero, check the hundred digit
		cmp #$21 				// it it is zero, exit (it is already 000)
		bne !+ 				  // otherwise it is 100,200 etc, so go and decrease
		jmp !Exit+

!:		lda #$26				// it is zero, so make it five
		sta $47ab
		dec $47aa 			// also decrement the 10s digit
		lda $47aa
		cmp #$21 				// is it zero?
		bcs !Exit+			// if greater than zero, exit
		lda #$2a 				// otherwise, write 9 to 10s
		sta $47aa
		dec $47a9 			// decrement 100s
		lda $47a9 
		cmp #$21 				// check if it is zero
		bcs !Exit+  		// if equal or greater than zero, exit
		lda #$21 				// otherwise, write zero to hundreds
		sta $47a9
		jmp !Exit+

// the rightmost digit is 5 (previously decremented)
!:		lda #$21
		sta $47ab

!Exit:
		rts 
}

// After collecting all crates, the door will open--------------------------------------
OpenDoor: {
		lda CurrentLevel 					// check and branch according to the level
		cmp #$01
		beq !Level1+
		cmp #$02
		beq !Level2+			
		cmp #$03
		beq !Level3+			
		cmp #$04
		beq !Level4+			
		// cmp #$05
		// beq !Level5+
		jmp !Level5+
		rts 

!Level1:
		PrintChar2Screen(0,$4400,36,20)
		PrintChar2Screen(0,$4400,36,21)
		jsr DoorOpeningSound
		jsr BorderChange
		rts

!Level2:
		PrintChar2Screen(0,$4400,3,20)
		PrintChar2Screen(0,$4400,3,21)
		jsr DoorOpeningSound
		jsr BorderChange
		rts

!Level3:
		PrintChar2Screen(0,$4400,35,04)
		PrintChar2Screen(0,$4400,35,05)
		jsr DoorOpeningSound
		jsr BorderChange
		rts

!Level4:
		PrintChar2Screen(0,$4400,20,20)
		PrintChar2Screen(0,$4400,20,21)
		jsr DoorOpeningSound
		jsr BorderChange
		rts

!Level5:
		PrintChar2Screen(0,$4400,19,04)
		PrintChar2Screen(0,$4400,19,05)
		jsr DoorOpeningSound
		jsr BorderChange
		rts
}

BorderChange: {
		ldx #$00
!:		stx $d020
		jsr delayfast
		inx 
		cpx #$0f
		bne !-
		lda #$00
		sta $d020
}

// Decrease time as time goes by--------------------------------------------------------
DecreaseTime: {
		dec Time_Counter						// decrement he time counter
		bne !Exit+ 									// check if it is if not zero, then exit
		jsr Decrease_Time_Status 		// if zero, then counter = 0, so decrease time status
		lda #$80 										// start the countdown again
		sta Time_Counter

!Exit:	rts 
}

//Playing sound effects------------------------------------------------------------
// channel 1
CollectSound:
		lda #>sfx_collect_crate
		ldy #<sfx_collect_crate
		ldx #$00
		jsr $9009
		rts 

AntiCrateSound:
		lda #>sfx_collect_anticrate
		ldy #<sfx_collect_anticrate
		ldx #$00
		jsr $9009
		rts 

CollectTimeSound: 
		lda #>sfx_collect_time
		ldy #<sfx_collect_time
		ldx #$00
		jsr $9009
		rts 

// channel 2
RobotMUShhoting:
		lda #>sfx_laser3
		ldy #<sfx_laser3
		ldx #$01
		jsr $9009
		rts 

RobotTShooting:
		lda #>sfx_laser3
		ldy #<sfx_laser3
		ldx #$01
		jsr $9009
		rts 

DoorOpeningSound:
		lda #>sfx_door_open
		ldy #<sfx_door_open
		ldx #$02
		jsr $9009
		rts 

// channel 3
PlayerShot:
		lda #>sfx_player_hit
		ldy #<sfx_player_hit
		ldx #$02
		jsr $9009
		rts 

// mute the sound by playing an empty track
MuteSound:
		lda #$09
		jsr $c000
		rts 

// Intro song and jingles play here-----------------------------------------------
IntroSong:{
		sei 
		lda #<lev  
		sta $0314
		lda #>lev 
		sta $0315
		lda #$00
		jsr $c000
		cli 
		rts 
lev:	inc $d019
!:		lda $d012
		cmp #$f0
		bne !-
		jsr $c003
		jmp $ea31
}

LevelSong: {
		sei 
		lda #<lev  
		sta $0314
		lda #>lev 
		sta $0315
		lda #$01
		jsr $c000
		cli 
		rts 
lev:	inc $d019
		jsr $c003
		jmp $ea31
}

GameOverSong: {
		sei 
		lda #<lev  
		sta $0314
		lda #>lev 
		sta $0315
		lda #$02
		jsr $c000
		cli 
		rts 
lev:	inc $d019
		jsr $c003
		jmp $ea31
}

// Initialize game variables------------------------------------------------------
Initialize: {

		ClearScreen($4400,0)				// clear screen
		lda #<SpritePointers 				// load the related pointers and coordinate addresses
		sta $a4  										// to ZP locations. Later, when levels are advanced,
		lda #>SpritePointers 				// the lo bytes will be incremented to point the level's
		sta $a5 										// addresses
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
		lda #[STATE_FACE_LEFT + STATE_WALK_LEFT]	// set initial direction as facing right
		sta Player_State        
		lda Idle_Timeout
		sta Idle_Time
		lda #$00 								// door closed at the beginning
		sta DoorOpenFlag
		lda #$00  							// initialize the crates collected
		sta CrateNumber
	  	lda Time_Max							// set the time to max at the beginning
		sta Time_Status 					// time max = $30 (48)
		lda #$51 									// and time status character is the full one
		sta Time_Status_Char
		lda #$01 									// the initial level is 1
		sta CurrentLevel
		lda #<TIMESTATUSADDR		// load the time status address to ZP
		sta TIME_LOOKUP
		lda #>TIMESTATUSADDR
		sta TIME_LOOKUP+1
		lda #<TIMESTATUSCOLOR		// load time status color to ZP
		sta TIME_COLOR_LOOKUP
		lda #>TIMESTATUSCOLOR
		sta TIME_COLOR_LOOKUP+1	

// this part, the values are taken from the address that ZP points
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
		lda #$10
		sta Collision_Timer
		ldx #$01
		lda RobotM_Bullet_Table,x 
		sta RobotMBullet_Trigger
		lda RobotU_Bullet_Table,x 
		sta RobotUBullet_Trigger
	 	DrawScreen2($a000,$4400)
		rts 
}

//Setup sprites------------------------------------------------------------------------------
SetupSprites: {
		lda #$ff         						// set multi-color bit
		sta $d01c										// for sprites 0-7
		lda #BLACK              		// multi-color register
		sta SPRITE_MULCOL_1    			// black 
		lda #WHITE									// and white
		sta SPRITE_MULCOL_2
		ldy #$00 								
	!:	lda ($a6),y 								// get the sprite positions from ZP pointer
		sta SpritePos,y 
		iny 
		cpy #$0f 										// do it for 8 sprites
		bne !-
		ldy #$00
	!:	lda ($a4),y  								// set sprite pointer --> VIC BANK + $800 (32 x 64)
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
		jsr LevelSong
		inc CurrentLevel 						// check current level
		lda CurrentLevel
		cmp #$06 										// if it is 6 (>5), then the game is over.
		bcc !+
		jmp GameOver_Good

!:		lda #%00001111 
		sta $d015 									// turn off bullet sprites
		PrintScreen(AdvanceLevelText1,$4400,7,13,1,26)
		PrintScreen(AdvanceLevelText2,$4400,7,14,1,26)
		PrintScreen(AdvanceLevelText3,$4400,7,15,1,26)
		WaitJoy2Fire() 							// wait for joystick fire button
		jsr MuteSound
!:		inc $479f										// increment level on screen
		ldx #$00 										// get the status bar characters
!Sc:	lda $4798,x  								// and store it to a table
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
		DrawScreen2($a3e8,$4400)
		lda #[STATE_FACE_RIGHT + STATE_WALK_RIGHT]	
		sta Player_State  
		jmp !+
!Level3Screen:
		DrawScreen2($a7d0,$4400)
		lda #[STATE_FACE_LEFT + STATE_WALK_LEFT]	
		sta Player_State  
		jmp !+
!Level4Screen:
		DrawScreen2($abb8,$4400)
		lda #[STATE_FACE_RIGHT + STATE_WALK_RIGHT]	
		sta Player_State 
		jmp !+
!Level5Screen:
		DrawScreen2($afa0,$4400)
		lda #[STATE_FACE_LEFT + STATE_WALK_LEFT]	
		sta Player_State 

!:
		ldx #$00 									// insert the status bar back to the row
!Sc:	lda StatusRow,x 
		sta $4798,x 			
		inx 
		cpx #$28
		bne !Sc-
		ldy #$00 									// and the characters' color info to their position
!Cl:	lda $4798,y 		  				// get the character value from screen position
		tax 
   		lda $5800,x 							// get the attribute of the character
   		sta $db98,y 
   		iny
   		cpy #$28
   		bne !Cl-
		ldx #$00 									// increment sprite pointers for the related level
!:		inc $a4 
		inx 
		cpx #$08
		bne !-
		ldy #$00
	!:	lda ($a4),y  								// and load sprite pointers
		sta Sprite_Pointer,y    
		iny 
		cpy #$08
		bne !-
		ldx #$00 										// increment sprite positions table pointer
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
		ldy #$01 									 // load the level's robot edge limits from table 
!:		lda ($aa),y  
		sta ROBOTBRIGHTEDGE,y 
		iny 
		cpy #$08
		bne !-
		inc $a8 										// load the level's door positions
		inc $a8 
		ldy #$00
		lda ($a8),y 
		sta DoorXValue
		iny 
		lda ($a8),y 
		sta DoorYValue
		inc $ac  									// load the level's background character
		ldy #$00 									// it will be replaced as the player gets the 
		lda ($ac),y  							// crates, anticrates and time capsules
		sta ScreenBGChar
		lda Idle_Timeout
		sta Idle_Time
		lda #$00 									// door closed at the beginning
		sta DoorOpenFlag
		lda #$00  								// initialize the crates collected
		sta CrateNumber
		lda CurrentLevel 					// load the robots bullet trigger coordinates
		tax 
		lda RobotM_Bullet_Table,x 
		sta RobotMBullet_Trigger
		lda RobotU_Bullet_Table,x 
		sta RobotUBullet_Trigger
		lda #$ff  								// turn on all sprites
		sta $d015 
		jmp INT   								// and jump to the game interrupts
}

//Info screen----------------------------------------------------------------------
InfoScreen:{
		DrawScreen2($b400,$4400)	// macros say it all :)
		WaitJoy2Fire()
		rts
}

//variables----------------------------------------------------
* = $1800 "Variables and tables"

// sprite colors table
SpriteColors:	.byte $0c, $04, $04, $04, $0f, $0c, $0c, $05

// Sprite pointers       Pl   RB   RM   RU   RT   BM   BU   BT
SpritePointers:		.byte $25, $29, $29, $28, $2a, $2c, $2c, $2d 			// level 1
					.byte $25, $29, $29, $28, $2a, $2c, $2c, $2d 			// level 2
					.byte $25, $29, $29, $28, $2a, $2c, $2c, $2d 			// level 3
					.byte $25, $29, $29, $28, $2a, $2c, $2c, $2d 			// level 4
					.byte $25, $29, $29, $28, $2a, $2c, $2c, $2d 			// level 5

// Starting sprite coordinates (x/y)
//												Player  RobotB  RobotM  RobotU  RobotT  
SpriteCoordinates:	.byte $8c,$cd,$1e,$cd,$1f,$7d,$61,$65,$54,$37,$00,$00,$00,$00,$00,$00 			// level 1
					.byte $16,$cd,$8c,$cd,$2d,$5d,$63,$75,$54,$37,$00,$00,$00,$00,$00,$00 			// level 2
					.byte $8c,$cd,$1e,$cd,$1d,$55,$90,$7d,$54,$37,$00,$00,$00,$00,$00,$00 			// level 3
					.byte $13,$cd,$91,$cd,$53,$ad,$53,$4d,$54,$37,$00,$00,$00,$00,$00,$00 			// level 4
					.byte $8c,$cd,$1e,$cd,$55,$8d,$53,$4d,$54,$37,$00,$00,$00,$00,$00,$00 			// level 5

// Door locations (char x/y values)
DoorLocation:		.byte $24,$16 		// level 1
					.byte $03,$16			// level 2
					.byte $23,$06			// level 3
					.byte $14,$16			// level 4
					.byte $13,$06			// level 5

// 	x coordinates						RBR,RBL,RMR,RML,RUR,RUL,RTR,RTL
RobotEdgeCoordinates:	.byte $9c,$0e,$33,$0e,$67,$45,$9c,$10 		// level 1
						.byte $9c,$0e,$3a,$1b,$76,$4d,$9c,$10 		// level 2  RobotM on top, RobotU in the middle
						.byte $9c,$0e,$2d,$0e,$9e,$7c,$9c,$10 		// level 3  RobotM on top, RobotU in the middle
						.byte $9c,$0e,$76,$35,$71,$3a,$9c,$10 		// level 4
						.byte $9c,$0e,$67,$44,$7b,$32,$9c,$10 		// level 5

RobotM_Bullet_Table:	.byte $00,$32,$20,$13,$37,$45 						// load to RobotUBullet_Trigger (x coord)
RobotU_Bullet_Table:	.byte $00,$60,$70,$90,$6f,$79

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
.label TIMESTATUSADDR	= $47be
.label TIMESTATUSCOLOR	= $dbbe

ROBOTBRIGHTEDGE:		.byte $00
ROBOTBLEFTEDGE:			.byte $00
ROBOTMRIGHTEDGE:		.byte $00
ROBOTMLEFTEDGE:			.byte $00
ROBOTURIGHTEDGE:		.byte $00
ROBOTULEFTEDGE:			.byte $00
ROBOTTRIGHTEDGE:		.byte $00
ROBOTTLEFTEDGE:			.byte $00

CurrentLevel:			.byte $00
Player_JumpIndex: 		.byte $00
Player_FallIndex:		.byte $00
Player_WalkIndex_R:		.byte $00
Player_WalkIndex_L:		.byte $00
Player_State: 			.byte $00
Player_WalkSpeed:		.byte $01
Collision_Temp:			.byte $00
JOY_ZP: 				.byte $00 
PlayerCharX:			.byte $00
PlayerCharY:			.byte $00
PlayerCharX_Coll:		.byte $00
PlayerCharY_Coll:		.byte $00
CrateCharValue:			.byte $43
TimeCapsuleCharValue:	.byte $44 
AntiCrateCharValue:		.byte $4c  
Idle_Time:				.byte $00
Idle_Timeout: 			.byte $14
CrateNumber:			.byte $00
Door_Status:			.byte $00
Robot_B_State:			.byte $01 		// 0=face left 	1=face right
Robot_M_State:			.byte $01 		// 0=face left 	1=face right
Robot_U_State:			.byte $00 		// 0=face left 	1=face right
Robot_T_State:			.byte $00 		// 0=down 	1=up
Robot_T_Timer:			.byte $00
Robot_M_Timer:			.byte $00
Robot_U_Timer:			.byte $00
Robot_T_Bullet_Flag:	.byte $00
Robot_M_Bullet_Flag:	.byte $00
Robot_U_Bullet_Flag:	.byte $00
Robot_M_Delay_Flag:		.byte $00
Robot_U_Delay_Flag:		.byte $00
RobotMBullet_Trigger:	.byte $00
RobotUBullet_Trigger:	.byte $00
DelayTemp:				.byte $00,$00

DoorOpenFlag:			.byte $00
DoorXValue:				.byte $00
DoorYValue:				.byte $00
Time_Status:			.byte $00 		// holds the time left
Time_Status_Char:		.byte $51			// starts with the full character
Time_Max:				.byte $30 		// time max is 60
Time_Counter:			.byte $70 		// timer value for the decrease time routine

Player_R_table:			.byte $20,$20,$20,$21,$21,$21,$22,$22,$22,$21,$21,$21
Player_L_table:			.byte $24,$24,$24,$25,$25,$25,$26,$26,$26,$25,$25,$25
PlayerJumpRSprite:		.byte $23
PlayerJumpLSprite:		.byte $27
PlayerStandRSprite:		.byte $21
PlayerStandLSprite:		.byte $25
Sprite_Collision:		.byte $00
Collision_Timer:		.byte $00
Collision_Flag:			.byte $00
ScreenBGChar:			.byte $00
Temp_x:					.byte $00
TimeRow_Temp:			.byte $00
TimeChar_Temp:			.byte $00

ScreenBGTable:			.byte $3e,$52,$58,$5e,$68			// character numbers to replace the crates etc.

JumpTable:     .fill 15, 4*sin(toRadians(90+i*360/60))
FallTable:      .fill 15, 4*sin(toRadians(0+i*360/60))
                .fill 16, $04

//JumpTable:                .byte $04, $04, $04, $03, $03, $03, $03, $02, $02, $02, $01, $01, $01, $01, $00

//FallTable:				.byte $00, $01, $01, $01, $01, $01, $02, $02, $02, $03, $03, $03, $04, $04, $04
//						.byte $04, $04, $04, $04, $04, $04, $04, $04
//						.byte $04, $04, $04, $04, $04, $04, $04, $04
//						.byte $04, $04, $04, $04, $04, $04, $04, $04

GameOverBadText: 		.text "sorry, the time is up."
GameOverGoodText1:		.text "congratulations."
GameOverGoodText2:		.text "you managed to finish in time"

AdvanceLevelText1:		.text "congratulations. you have "
AdvanceLevelText2:		.text "advanced to the next level"
AdvanceLevelText3:		.text "  press fire to continue  "

ScreenLSB:  	.fill 25, <[$4400 + i * $28]		// y addresses of the 1st column
ScreenMSB:		.fill 25, >[$4400 + i * $28]

StatusRow:		.fill 40, 0 		// status row temporary location (between levels)
Score_temp:		.fill 4,0 			// the charset array to be used as buffer

// sound effects
sfx_collect_crate: 		.byte $00,$20,$ea,$11,$14,$03,$8d,$01,$03,$02,$15,$29
sfx_collect_anticrate:	.byte $00,$20,$ea,$11,$0c,$03,$8d,$01,$03,$02,$15,$29
sfx_laser1: 			.byte $07,$00,$f7,$42,$53,$f7,$e1,$08,$0f,$01,$13,$01
sfx_laser2: 			.byte $00,$00,$f7,$21,$28,$f7,$e1,$04,$1d,$01,$0c,$01
sfx_laser3:				.byte $06,$23,$f5,$12,$10,$0e,$fe,$03,$04,$01,$15,$01
sfx_door_open:			.byte $00,$00,$b7,$41,$0f,$04,$fa,$10,$08,$01,$48,$28
sfx_player_hit:			.byte $00,$00,$f8,$63,$08,$36,$99,$04,$0b,$01,$00,$08
sfx_collect_time:		.byte $00,$00,$f8,$44,$20,$36,$1d,$02,$11,$01,$00,$08
// Sound effects file---------------------------------------------------------------------------

*=$4800 "Sprite Data"
.import binary "Sprites_R1.bin"

* = $5000 "Character Set Data"
.import binary "Platform (1x1)_7 - Chars.bin"

* = $5800  "Character Set Attrib"
.import binary "Platform (1x1)_7 - CharAttribs_L1.bin"

* = $6000 "Bitmap screen"
.import binary "Jump'n'crate_bitmap.prg",2

* = $9000 "External sound driver"
.import c64 "sfxdriver-9000-standard.prg"

* = $a000 "Map Data"
.import binary "Platform (1x1)_7 - (8bpc, 40x125) Map.bin"

* = $b400 "Info screen"
.import binary "Platform-info - (8bpc, 40x25) Map.bin"

* = $b800 "Final (success) screen"
.import binary "Final1 - (8bpc, 40x25) Map.bin"

* = $bc00 "Final (fail) screen"
.import binary "Final2 - (8bpc, 40x25) Map.bin"

*=$c000 "Song and jingles"
.import c64 "jumpncratesong.prg"

// to be discarded, experimental only
// sfx_explosmall: 	.byte $03,$00,$fa,$88,$12,$fa,$e5,$04,$14,$01,$00,$00
// sfx_stars: 			.byte $1f,$00,$ca,$45,$32,$cf,$0b,$0c,$08,$04,$55,$25
// sfx_ui: 					.byte $00,$00,$c7,$41,$19,$7f,$b8,$02,$2e,$01,$00,$08
// sfx_bounce: 			.byte $06,$20,$ea,$41,$07,$03,$fe,$03,$04,$0b,$15,$02
// sfx_laser: 			.byte $07,$00,$f7,$42,$53,$f7,$e1,$08,$0f,$01,$13,$01
// sfx_collect: 		.byte $0f,$00,$f9,$41,$1b,$11,$bc,$03,$05,$07,$0f,$22
// sfx_siren: 			.byte $00,$00,$eb,$41,$17,$04,$fa,$10,$08,$06,$48,$29
// sfx_fireecho: 		.byte $03,$00,$ea,$48,$12,$fa,$ae,$03,$06,$0a,$24,$28