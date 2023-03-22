* = * "PlayerControl"

// Player control routine------------------------------------
Player_Walk: {

		lda JOY_PORT_2				// load Joyport2 state
		sta JOY_ZP				// and save it to variable
		lda JOY_ZP
		cmp #%01111111				// all on means no switch activated,so less then $7f
		bcc !up+				// indicates there is a movement 
		
		dec Idle_Time
		lda Idle_Time
		beq !+
		jmp !++

!:		lda Idle_Timeout
		sta Idle_Time		

		lda #$01 										// if there is no mvement
		sta Player_WalkIndex_L			// load the standing sprite index
		sta Player_WalkIndex_R			//and skip movement
		lda Player_State
		and #STATE_FACE_LEFT
		bne !faceright+ 
		lda PlayerStandRSprite 
		sta Sprite_Pointer
		jmp !+

		!faceright:
		lda Player_State
		and #STATE_FACE_RIGHT
		bne !+ 
		lda PlayerStandLSprite 
		sta Sprite_Pointer

!:		//PrintByteScreen(Idle_Time,16,0,1)
		jmp !SkipMovement+

!up:	lda Idle_Timeout
		sta Idle_Time
		//PrintByteScreen(Idle_Time,16,0,1)
		lda Player_State				// if player is already jumping or falling
		and #[STATE_FALL + STATE_JUMP]	// then don't jump or fall again
		bne !+
		lda JOY_ZP					// check joystick
		and #JOY_UP					// for up position
		bne !+ 
		lda Player_State	
		ora #STATE_JUMP					// set jump =1 (switch on the bit)
		sta Player_State				// for the player state
		lda #$00 					// and reset the Jump index
		sta Player_JumpIndex
		//jsr JumpSound
		jmp !left+ 					// and then continu checking other directions
		!:

!left:	
		lda JOY_ZP						//check 00000100 -> left =0
		and #JOY_LT					// check against 1
		beq !Skip+					// 1&0=0  if result is 0, then move left
		jmp !right+ 					// otherwise, check right

!Skip:	
		lda Player_State	
		and #[255 - STATE_FACE_RIGHT - STATE_WALK_RIGHT]
		ora #[STATE_FACE_LEFT + STATE_WALK_LEFT]

		sta Player_State
		sec 
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
		cmp #$d0						// is it floor? If yes, then continue walking 
		bcs !EdgeCheck+	  	   			 // and check screen edges

						// if you are here, there is no jump, player walking 
						// and there is no floor beneath the player
		lda Player_State				// so make the state revision
		ora #STATE_FALL					// and start fall state (switch on fall state)
		sta Player_State
		lda #$00 						// reset the fall index
		sta Player_FallIndex  			// at this point, state = left & falling (18=$12)
		//jsr FallSound					// or left & walking & falling (22=$16 ) 

		//Check screen edge
!EdgeCheck:	
		lda Player_x
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
		sta Sprite_Pointer		// (standing and facing left)
		jmp !++							// and finish moving
!:	lda PlayerJumpLSprite			// or load the jumping sprite
		sta Sprite_Pointer		

!:		jmp !SkipMovement+				// and finish moving

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

!:	ldx Player_WalkIndex_L			// load the index
		lda Player_L_table,x 			// get the number from the table in accordance with the index
		sta Sprite_Pointer		// save it to the sprite pointer for the frame
		jsr Walking_Sound_Left			// play the sound effect
		jmp !SkipMovement+				// left walking completed, exit the checking routine

!right:	lda JOY_ZP						//check 00001000 -> right =0
		and #JOY_RT 					// check against 1
		beq !Skip+ 						// 1&0=0  if result is 0, then move right
		jmp !SkipMovement+ 				// otherwise finish movement routine

!Skip:	
		lda Player_State				// ensure that the player state is face left & walk left
		and #[255 - STATE_FACE_LEFT - STATE_WALK_LEFT]
		ora #[STATE_WALK_RIGHT + STATE_FACE_RIGHT]
		sta Player_State

		clc 							// move player right in accordance with walkspeed
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
		cmp #$d0								// is it floor?	(the characters >$50 are walkable)
		bcs !EdgeCheck+	 	  		// if no, then check screen edges

						// if you are here, there is no jump and 
						// there is no floor beneath the player
		lda Player_State				// so make the state revision
		ora #STATE_FALL					// and start fall state  (switch on fall state)
		sta Player_State
		lda #$00 								// reset the fall index
		sta Player_FallIndex 		// at this point, state = right & falling (34=$22)
									// or right & walking & falling (40=$28 ) 

		//Check screen edge
!EdgeCheck:	
		lda Player_x
		cmp #RIGHT_SCREEN_EDGE		// did he reach the right edge coordinate?
		bcc !SkipEdgeCheck+				// if no, skip
		lda #RIGHT_SCREEN_EDGE		// if yes, limit the coordinate
		sta Player_x

!SkipEdgeCheck:
		lda Player_State				// check player state whether he
		and #[STATE_FALL + STATE_JUMP]	// is falling or jumping
		beq !PlayerDone+ 				// if not, go progress walking
		lda Player_State				// check the state
		and #STATE_FALL
		beq !+							
		lda PlayerJumpRSprite		// if falling, load the falling sprite 
		sta Sprite_Pointer			// (standing and facing left)
		jmp !++									// and finish moving
!:	lda PlayerJumpRSprite		// or load the jumping sprite
		sta Sprite_Pointer			// and save it to the sprite pointer

!:	jmp !SkipMovement+			// and finish moving			

		// this part is walking right
!PlayerDone:	
		lda Player_State				// ensure that the player state is face right & walk right
		and #[255 - STATE_FACE_LEFT - STATE_WALK_LEFT]
		ora #[STATE_WALK_RIGHT + STATE_FACE_RIGHT]
		sta Player_State
		inc Player_WalkIndex_R 	// increment walking right index
		lda Player_WalkIndex_R	// for the right sprite frame
		cmp #$0c  							// did it reach 12?
		bne !+									// if no, continue with the sprite
		lda #$00 								// if yes, then reset it
		sta Player_WalkIndex_R			
!:		ldx Player_WalkIndex_R	// load the index
		lda Player_R_table,x 		// get the number from the table in accordance with the index
		sta Sprite_Pointer			// save it to the sprite pointer for the frame
		jsr Walking_Sound_Right	// play the sound effect
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
		lda Player_y				// get the player y coordinate
		sec 
		sbc JumpTable,x 			// decrement the y coordinate by the jump table value
		cmp #56
		bcc !+
		sta Player_y				// and store it to the player y coordinate
!:  inx 					// increment the jump index
		cpx #$0f 				// check if it reached 12 (limit of jump index)
		bne !+ 					// if no, continue
		lda Player_State 			// if yes, jump will be finalized
		and #[255 - STATE_JUMP] 		// remove jump state from player state
		ora #[STATE_FALL]			// and start fall sequence
		sta Player_State
		ldx #$00 				// and reset the jump index (if completed)
!:	txa
		sta Player_JumpIndex			// save the current index

!Skip:	// this part is fall back
		lda Player_State			// check the player state
		and #STATE_FALL				// if there is no fall
		beq !Exit+				// then skip to the ending part

		jsr GetBelowChar
		lda BelowCharVal			//look at below character
		cmp #$d0				// is it floor?
		bcs !FinJ+				// if yes, then finalize jump

		lda Player_FallIndex			// otherwise continue fall
		tax
		lda Player_y				// get the player y coordinate
		clc 
		adc FallTable,x 			// increment the y coordinate by the jump table value 
		sta Player_y				// and store it to the player y coordinate
		inx 					// increment the jump index
		cpx #$22 				// check if it reached 12 (limit of jump index)
		bne !++ 				// if no, continue and exit by storing the fall index

		// if you are here, the fall from the jump has ended
		// but there is no floor beneath, so fall down until there is floor
!:	jsr delayfast 				// insert a delay, since this loop is out of the main delay routine
		inc Player_y 				// increment player y coordinate
		inc Player_y 				// increment player y coordinate
		inc Player_y 				// increment player y coordinate -> Fall by 3 pixels
		jsr CalcSpriteChar			// get the character coordinates of the player
		jsr Expand_Sprites			// calculate the final (hardware) sprite coordinates
		jsr GetBelowChar 			// loot at the below character
		lda BelowCharVal 					
		cmp #$d0 				// if it is space, rinse and repeat
		bcc !- 					// until there is floor

		//fall completed. load the sprite for standing still

	!FinJ:	
		jsr AdjustSpriteYCoor
		lda Player_State 		//load the player state
		and #[255 - STATE_FALL] 	// remove the fall state from it
		sta Player_State 		// and store it to the palyer state
		lda Player_State 		// check the final state
		cmp #$14 			// check if it is $14, it is walking left and facing left
		bne !yl+ 			// if no, skip to checking right
		ldy PlayerStandLSprite		// if yes, load the standing left postion sprite
		sty Sprite_Pointer		// and store it to the sprite pointer
!yl:		
		cmp #$28 			// check if it is $28, it is walking right and facing right
		bne !yr+			// if no, skip to storing fall index
		ldy PlayerStandRSprite 		// if yes, load the standing right postion sprite
		sty Sprite_Pointer		// and store it to the sprite pointer

!yr:		
		ldx #$00 			// if you are here, then reset the fall index
!:		txa
		sta Player_FallIndex 		// store it to the fall index

!Exit:
		rts
	}