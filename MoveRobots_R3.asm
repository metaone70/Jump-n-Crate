* = * "MoveRobots"
// move the robots according to the tables
MoveRobots: {
		// jsr RobotsMovingSound
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
		cmp ROBOTBLEFTEDGE 				// did it reach the left edge?
		bne !RobotM+ 						// if not, go and check RobotM
		lda #$01
		sta Robot_B_State 			// yes, so turn right (state=1)
		lda #$29 								// change sprite pointer 
		sta Sprite_Pointer+1 		// for sprite #1
		jmp !RobotM+

!RobotBGoingRight: 					 //Robot B is going right
		lda Player_y				// check if player is at the bottom row
		cmp RobotB_y
		bne !Skip+ 	

		lda RobotB_x
		sec 						// subtract RobotB Y pos from Player Y pos 
		sbc Player_x   				// if result is negative, then Player is
		bmi !Skip+  				// on the right of the RobotB and RobotB is going left
		lda #$00
		sta Robot_B_State
		jmp !RobotM+
		
!Skip:	inc RobotB_x  				// RobotB is going right 

// RobotB Right Edge Check
		lda RobotB_x
		cmp ROBOTBRIGHTEDGE 		// did it reach the right edge?
		bne !RobotM+ 				// if not, go and check RobotM
		lda #$00
		sta Robot_B_State 			// yes, so turn left (state=0)
		lda #$28 					// change sprite pointer 
		sta Sprite_Pointer+1 		// for sprite #1

!RobotM:  //-----------------------
		lda Robot_M_Delay_Flag
		beq !Skip+

		lda Robot_M_State			// check RobotM state
		bne !++						// if not zero, then going right 
		dec RobotM_x  				// RobotB is going left 
		lda RobotM_x
		cmp ROBOTMLEFTEDGE 		// did it reach the left edge?
		bne !+ 						// if not, go and check RobotU
		lda #$01
		sta Robot_M_State 			// yes, so turn right (state=1)
		lda #$29 					// change sprite pointer 
		sta Sprite_Pointer+2 		// for sprite #2
!:		lda #$00
		sta Robot_M_Delay_Flag
		jmp !RobotU+

!:  //Robot M right
		inc RobotM_x  				// RobotM is going right 
		lda RobotM_x
		cmp ROBOTMRIGHTEDGE 		// did it reach the right edge?
		bne !+ 						// if not, go and check RobotU
		lda #$00
		sta Robot_M_State 			// yes, so turn left (state=0)
		lda #$28 					// change sprite pointer 
		sta Sprite_Pointer+2 		// for sprite #2
!:		lda #$00
		sta Robot_M_Delay_Flag
		jmp !RobotU+

!Skip: 	lda #$01
		sta Robot_M_Delay_Flag

!RobotU:  //-------------------------------------
		lda Robot_U_Delay_Flag
		beq !Skip+

		lda Robot_U_State			// check RobotB state
		bne !++						// if not zero, then going right 
		dec RobotU_x  				// RobotB is going left 
		lda RobotU_x
		cmp ROBOTULEFTEDGE 		// did it reach the left edge?
		bne !+ 						// if not, go and check RobotM
		lda #$01
		sta Robot_U_State 			// yes, so turn right (state=1)
		lda #$29 					// change sprite pointer 
		sta Sprite_Pointer+3 		// for sprite #2
!:		lda #$00
		sta Robot_U_Delay_Flag
		jmp !RobotT+

!:  //Robot U right
		inc RobotU_x  				// RobotB is going right 
		lda RobotU_x
		cmp ROBOTURIGHTEDGE 		// did it reach the right edge?
		bne !+ 						// if not, go and check RobotM
		lda #$00
		sta Robot_U_State 			// yes, so turn left (state=0)
		lda #$28 					// change sprite pointer 
		sta Sprite_Pointer+3 		// for sprite #1
!:		lda #$00
		sta Robot_U_Delay_Flag
		jmp !RobotT+

!Skip: 	lda #$01
		sta Robot_U_Delay_Flag

!RobotT:  //--------------------------------
		lda Robot_T_State				// check RobotB state
		bne !+							// if not zero, then going right 
		dec RobotT_x 					// going left
		lda RobotT_x
		cmp ROBOTTLEFTEDGE
		bne !++
		lda #$01
		sta Robot_T_State
		//jsr RobotsMovingSound
		jmp !++

!:	//Robot T right
		inc RobotT_x
		lda RobotT_x
		cmp ROBOTTRIGHTEDGE
		bne !+
		lda #$00
		sta Robot_T_State

!:		inc Robot_T_Timer	
		lda Robot_T_Timer
		cmp #$20
		bne Robot_T_Bullet

		lda Sprite_Pointer+4
		cmp #$2a
		bne !+
		lda #$2b
		sta Sprite_Pointer+4
		jmp !++

!:		lda #$2a
		sta Sprite_Pointer+4
!:		lda #$00
		sta Robot_T_Timer

// Robot_T bullet movement
Robot_T_Bullet:
		lda Robot_T_Bullet_Flag
		bne !Move_Robot_T_Bullet+

		lda RobotT_x
		sta RobotTBul_x
		lda RobotT_y
		clc
		adc #$05
		sta RobotTBul_y
		lda #$01
		sta Robot_T_Bullet_Flag
		jsr RobotTShooting
		jmp Robot_M_Bullet

!Move_Robot_T_Bullet:
		inc RobotTBul_y
		inc RobotTBul_y
		lda RobotTBul_y 
		cmp #$d7 
		bcc !+
		lda #$00
		sta RobotTBul_x
		sta RobotTBul_y
		lda #$00
		sta Robot_T_Bullet_Flag
		jmp Robot_M_Bullet

!:		sta RobotTBul_y

// Robot_M bullet movement -> towards right
Robot_M_Bullet:
		lda Robot_M_Bullet_Flag
		bne !Move_Robot_M_Bullet+ 

		lda Robot_M_State				// check it the Robot_M is looking right
		bne !+							// 0=face left 	1=face right
		jmp Robot_U_Bullet		

!:		lda RobotM_x
		cmp RobotMBullet_Trigger
		bne Robot_U_Bullet
		lda RobotM_x
		clc 
		adc #$03
		sta RobotMBul_x
		lda RobotM_y 
		sta RobotMBul_y
		lda #$01
		sta Robot_M_Bullet_Flag
		jsr RobotMUShhoting
		jmp Robot_U_Bullet

!Move_Robot_M_Bullet:
		inc RobotMBul_x
		inc RobotMBul_x
		lda RobotMBul_x
		cmp #$a2
		bcc !+
		lda #$00
		sta RobotMBul_x
		sta RobotMBul_y
		sta Robot_M_Bullet_Flag
		jmp Robot_U_Bullet
!:		sta RobotMBul_x

// Robot_U bullet movement
Robot_U_Bullet:
		lda Robot_U_Bullet_Flag
		bne !Move_Robot_U_Bullet+ 

		lda Robot_U_State				// check it the Robot_U is looking right
		beq !+							// 0=face left 	1=face right
		jmp Exit

!:		
		lda RobotU_x
		cmp RobotUBullet_Trigger
		bne Exit
		lda RobotU_x
		sec  
		sbc #$03
		sta RobotUBul_x
		lda RobotU_y
		sta RobotUBul_y
		lda #$01
		sta Robot_U_Bullet_Flag
		jsr RobotMUShhoting
		jmp Exit

!Move_Robot_U_Bullet:
		dec RobotUBul_x
		dec RobotUBul_x
		lda RobotUBul_x
		cmp #$0b
		bcs !+
		lda #$00
		sta RobotUBul_x
		sta RobotUBul_y
		sta Robot_U_Bullet_Flag
		jmp Exit

!:		sta RobotUBul_x

Exit:	rts 
}