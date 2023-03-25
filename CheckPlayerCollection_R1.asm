* = * "CheckPlayerCollection"

// Check if the player collects a crate--------------------------------------------------
Check_Player_Collection: {

		jsr GetOnChar						// get the character value that the sprite is on
		lda OnCharVal						// and load it to accumulator
		cmp CrateCharValue					// is it a crate? 
		bne !CheckTimeCapsule+				// if no, check if it is AntiCrate
		lda PlayerCharX 					// if yes, then we need to change it to background value 
		tay 								// get the char x coordinate and transfer it to y
		lda #$3e 							// load the background character value
		sta (SCREEN_LOOKUP2),y 				// and store it to ZP address -> this puts the background char on screen
												// SCREENLOOKUP2 is used here
		lda SCREEN_LOOKUP2 					// we need to find the COLOR RAM for the character
		sta CHARCOLORRAM 					// we are basically adding $94 -> $d800 - $4400 ($d8 - $44 = $94 for ZP)
		lda SCREEN_LOOKUP2+1
		clc 
		adc #$94
		sta CHARCOLORRAM+1 					// and we store the color ram address to CHARCOLORRAM ZP

		ldx #$3f 							// the position of the character is $39 in the characterset
		lda $5800,x 						// we get the $39th attrib from table
		tax 								// and transfer it to x
		lda PlayerCharX 					// we get the char x value of sprite
		tay
		txa 								// and find the exact color ram location
		sta (CHARCOLORRAM),y 				// and store the attrib data

		jsr AddScore  						// since we get a crate, we need to increase the score
		jsr CollectSound 					// and some collection sound
		inc CrateNumber

		lda CrateNumber
		cmp #TotalCrateNumber
		bne !Skip+
		lda #$01
		sta DoorOpenFlag
		jmp OpenDoor

!CheckTimeCapsule:
		cmp TimeCapsuleCharValue		// check if it is AntiCrate
		bne !CheckAntiCrate+

		lda PlayerCharX 				// if yes, then we need to change it to background value 
		tay 							// get the x char coordinate and transfer it to y
		lda #$3e 						// load the background character value
		sta (SCREEN_LOOKUP2),y 			// and store it to ZP address -> this puts the background char on screen
										// SCREENLOOKUP2 is used here
		lda SCREEN_LOOKUP2 				// we need to find the COLOR RAM for the character
		sta CHARCOLORRAM 				// we are basically adding $94 -> $d800 - $4400 ($d8 - $44 = $94 for ZP)
		lda SCREEN_LOOKUP2+1
		clc 
		adc #$94
		sta CHARCOLORRAM+1 				// and we store the color ram address to CHARCOLORRAM ZP

		ldx #$3f 						// the position of the character is $39 in the characterset
		lda $5800,x 					// we get the $39th attrib from table
		tax 							// and transfer it to x
		lda PlayerCharX 				// we get the char x value of sprite
		tay
		txa 							// and find the exact color ram location
		sta (CHARCOLORRAM),y 			// and store the attrib data

		jsr CollectTimeSound
		jsr Increase_Time_Status
!:		jmp !Skip+

!CheckAntiCrate:
		cmp AntiCrateCharValue			// check if it is AntiCrate
		bne !Skip+

		lda PlayerCharX 				// if yes, then we need to change it to background value 
		tay 							// get the x char coordinate and transfer it to y
		lda #$3e 						// load the background character value
		sta (SCREEN_LOOKUP2),y 			// and store it to ZP address -> this puts the background char on screen
										// SCREENLOOKUP2 is used here
		lda SCREEN_LOOKUP2 				// we need to find the COLOR RAM for the character
		sta CHARCOLORRAM 				// we are basically adding $94 -> $d800 - $4400 ($d8 - $44 = $94 for ZP)
		lda SCREEN_LOOKUP2+1
		clc 
		adc #$94
		sta CHARCOLORRAM+1 				// and we store the color ram address to CHARCOLORRAM ZP

		ldx #$3f 						// the position of the character is $39 in the characterset
		lda $5800,x 					// we get the $39th attrib from table
		tax 							// and transfer it to x
		lda PlayerCharX 				// we get the char x value of sprite
		tay
		txa 							// and find the exact color ram location
		sta (CHARCOLORRAM),y 			// and store the attrib data

		jsr AntiCrateSound
		jmp Decrease_Time_Status

!Skip:	rts 
	}