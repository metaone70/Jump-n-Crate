* = * "MoveRobots"
// move the robots according to the tables
MoveRobots: {

.eval var RobotB_x		= SpritePos+2
.eval var RobotB_y		= SpritePos+3
.eval var RobotM_x		= SpritePos+4
.eval var RobotM_y		= SpritePos+5
.eval var RobotU_x		= SpritePos+6
.eval var RobotU_y		= SpritePos+7
.eval var RobotT_x		= SpritePos+8
.eval var RobotT_y		= SpritePos+9
.eval var RobotBBul_x 	= SpritePos+10
.eval var RobotBBul_y 	= SpritePos+11
.eval var RobotMBul_x 	= SpritePos+12
.eval var RobotMBul_y 	= SpritePos+13
.eval var RobotTBul_x 	= SpritePos+14
.eval var RobotTBul_y 	= SpritePos+15

		lda Robot_B_State				// check RobotB state
		bne !RobotBGoingRight+			// if not zero, then going right 

//RobotB is going left
		lda Player_y					// check if player is at the bottom row
		cmp RobotB_y
		bne !Skip+ 						// if not, continue operation

		lda Player_x
		sec 							// subtract RobotB Y pos from Player Y pos 
		sbc RobotB_x 					// if result is positive, then Player is
		bmi !Skip+  					// on the right of the RobotB and RobotB is going left
		lda #$01 						// so change the RobotB state to face Player	
		sta Robot_B_State
		jmp !RobotBGoingRight+			// and continue with right check
		
!Skip:	dec RobotB_x  					// otherwise, RobotB is going left 

// RobotB Left Edge Check
		lda RobotB_x
		cmp #ROBOTBLEFTEDGE 		// did it reach the left edge?
		bne !RobotM+ 						// if not, go and check RobotM
		lda #$01
		sta Robot_B_State 			// yes, so turn right (state=1)
		lda #$89 								// change sprite pointer 
		sta Sprite_Pointer+1 		// for sprite #1
		jmp !RobotM+

!RobotBGoingRight: 					 //Robot B is going right
		lda Player_y					// check if player is at the bottom row
		cmp RobotB_y
		bne !Skip+ 	

		lda RobotB_x
		sec 							// subtract RobotB Y pos from Player Y pos 
		sbc Player_x   					// if result is negative, then Player is
		bmi !Skip+  					// on the right of the RobotB and RobotB is going left
		lda #$00
		sta Robot_B_State
		jmp !RobotM+
		
!Skip:	inc RobotB_x  					// RobotB is going right 

// RobotB Right Edge Check
		lda RobotB_x
		cmp #ROBOTBRIGHTEDGE 		// did it reach the right edge?
		bne !RobotM+ 				// if not, go and check RobotM
		lda #$00
		sta Robot_B_State 			// yes, so turn left (state=0)
		lda #$88 					// change sprite pointer 
		sta Sprite_Pointer+1 		// for sprite #1

!RobotM:  //-----------------------
		lda Robot_M_State				// check RobotM state
		bne !+									// if not zero, then going right 
		
		dec RobotM_x  					// RobotB is going left 
		lda RobotM_x
		cmp #ROBOTMLEFTEDGE 		// did it reach the left edge?
		bne !RobotU+ 						// if not, go and check RobotU
		lda #$01
		sta Robot_M_State 			// yes, so turn right (state=1)
		lda #$89 								// change sprite pointer 
		sta Sprite_Pointer+2 		// for sprite #2
		jmp !RobotU+

!:  //Robot M right
		inc RobotM_x  					// RobotM is going left 
		lda RobotM_x
		cmp #ROBOTMRIGHTEDGE 		// did it reach the right edge?
		bne !RobotU+ 						// if not, go and check RobotU
		lda #$00
		sta Robot_M_State 			// yes, so turn left (state=0)
		lda #$88 								// change sprite pointer 
		sta Sprite_Pointer+2 		// for sprite #2

!RobotU:  //-------------------------------------
		lda Robot_U_State				// check RobotB state
		bne !+									// if not zero, then going right 
		dec RobotU_x  					// RobotB is going left 
		lda RobotU_x
		cmp #ROBOTULEFTEDGE 		// did it reach the left edge?
		bne !RobotT+ 						// if not, go and check RobotM
		lda #$01
		sta Robot_U_State 			// yes, so turn right (state=1)
		lda #$89 								// change sprite pointer 
		sta Sprite_Pointer+3 		// for sprite #2
		lda #$00

!:  //Robot U right
		inc RobotU_x  					// RobotB is going right 
		lda RobotU_x
		cmp #ROBOTURIGHTEDGE 		// did it reach the right edge?
		bne !RobotT+ 						// if not, go and check RobotM
		lda #$00
		sta Robot_U_State 			// yes, so turn left (state=0)
		lda #$88 								// change sprite pointer 
		sta Sprite_Pointer+3 		// for sprite #1

!RobotT:  //--------------------------------
		lda Robot_T_State				// check RobotB state
		bne !+									// if not zero, then going right 
		dec RobotT_x
		lda RobotT_x
		cmp #ROBOTTLEFTEDGE
		bne !++
		lda #$01
		sta Robot_T_State
		//jsr RobotsMovingSound
		jmp !++

!:	//Robot T right
		inc RobotT_x
		lda RobotT_x
		cmp #ROBOTTRIGHTEDGE
		bne !+
		lda #$00
		sta Robot_T_State

!:		inc Robot_T_Timer	
		lda Robot_T_Timer
		cmp #$20
		bne !Exit+ 

		lda Sprite_Pointer+4
		cmp #$8a
		bne !+
		lda #$8b
		sta Sprite_Pointer+4
		jmp !++

!:		lda #$8a
		sta Sprite_Pointer+4
!:		lda #$00
		sta Robot_T_Timer

!Exit:
		rts 
}