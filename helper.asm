// Macro List:
// ClearScreen(screen,clearByte)
// ClearColorRam(clearByte)
// SetBorderColor(color)
// SetBackgroundColor(color)
// PrintScreen(message,screenstart, xcoor,ycoor,color,textlength)
// PrintByteScreen(byteaddress,xcoor,ycoor,color)
// PrintByteScreen2(bytevalue,screenstart,xcoor,ycoor,color)
// DrawScreen(screenData,colorData,screenAddress)
// DrawScreen2(screenData,screenAddress)
// DrawBitmap(scrData,scrRam,colData)
// WaitSpaceKey()
// WaitAKey()
// StoreState()
// RestoreState()
// waitForRasterLine(line)


.macro ClearScreen(screen,value) {
	lda #value	// space
	ldx #0
!loop:
	sta screen, x
	sta screen + $100, x
	sta screen + $200, x
	sta screen + $300, x
	inx
	bne !loop-
}

.macro ClearColorRam(color) {
	lda #color
	ldx #0
!loop:
	sta $d800, x
	sta $d800 + $100, x
	sta $d800 + $200, x
	sta $d800 + $300, x
	inx
	bne !loop-
}

.macro SetBorderColor(color) {
	lda #color
	sta $d020
}

.macro SetBackgroundColor(color) {
	lda #color
	sta $d021
}

.macro WaitSpaceKey() {
!:	lda $dc01
	and #$10
	bne !-
}

.macro WaitAKey() {
loop:   jsr $ff9f                // waits for any key
        jsr $ffe4       
        beq loop 
}

.macro WaitJoy1Fire() {
!Joy1:	lda $dc01
	lsr 
	lsr 
	lsr 
	lsr 
	lsr 
	bcc !+
	jmp !Joy1-
!:		
}

.macro WaitJoy2Fire() {
!Joy2:	lda $dc00
	lsr 
	lsr 
	lsr 
	lsr 
	lsr 
	bcc !+
	jmp !Joy2-
!:		
}


.macro StoreState() {
		pha //A
		txa 
		pha //X
		tya 
		pha //Y
}

.macro RestoreState() {
		pla //A
		tay
		pla //X
		tax 
		pla //Y
}

.macro waitForRasterLine(line) {
		lda #line
!loop:		cmp $d012
		bne !loop-	
}

.macro PrintScreen(message,screenstart, xcoor,ycoor,color,textlength) {
	.var textAddress = screenstart + (ycoor*40)+xcoor
	.var colorAddress = $d800+(ycoor*40) + xcoor
	StoreState()
	ldx #$00
!loop:  lda message,x
	sta textAddress,x 
	inx
	cpx #textlength	
	bne !loop-

	ldx #$00
!loop:	lda #color 
	sta colorAddress,x 
	inx
	cpx #textlength
	bne !loop-
	RestoreState()
}

.macro DrawScreen(screenData,colorData,screenAddress) {
	lda #$00
	sta $f7
	sta $f9			
	sta $fb
	sta $fd			

	lda #>screenAddress	
	sta $fe 
	lda #$d8
	sta $f8			
	lda #<colorData
	sta $f9			
	lda #>colorData
	sta $fa
	lda #<screenData
	sta $fb
	lda #>screenData
	sta $fc
      	
        ldx #$00
loop1:       
        ldy #$00
loop2:     
        lda ($fb),y             
        sta ($fd),y            
        lda ($f9),y
        sta ($f7),y
        iny
        bne loop2
        inc $fc
        inc $fe
        inc $fa
        inc $f8
        inx
        cpx #$04
        bne loop1

//screen:
//        .byte    $20,$20
//color:
//        .byte    $01,$01
}

// draw full map to screen
.macro DrawScreen2(screenData,screenAddress) {

	lda #<screenData
	sta $f7
	lda #>screenData
	sta $f8
	lda #<screenAddress
	sta $f9 
	lda #>screenAddress
	sta $fa
    	
        ldx #$00
loop1:       
        ldy #$00
loop2:     
        lda ($f7),y             
        sta ($f9),y            
        iny
        bne loop2
        inc $f8
        inc $fa 
        inx
        cpx #$04
        bne loop1

   	// this part is to set the color of multicolor characters
   	// we first get the character # from secreen, we find its attrib 
   	// from the attrib table and store it to its color RAM
   	// and we do it 4 times (4x256=1000 characters)
   	ldy #$00
!:	lda screenAddress,y 		// get the character value from screen position
	tax 
   	lda $5800,x 			// get the attribute of the character
   	sta COLOR_RAM,y 		// and store it to its color RAM
   	lda screenAddress+$100,y
   	tax 
   	lda $5800,x 
   	sta COLOR_RAM+$100,y
   	lda screenAddress+$200,y
   	tax 
   	lda $5800,x 
   	sta COLOR_RAM+$200,y
   	lda screenAddress+$300,y
   	tax 
   	lda $5800,x 
   	sta COLOR_RAM+$300,y
   	iny 
   	bne !-
}


.macro DrawBitmap(scrData,scrRam,colData) {
	.var colRam = $d800
	.var backGroundColor = colData + $03e8

       	ldx #$00
!loop:  lda scrData,x
        sta scrRam,x
        lda scrData+$100,x
        sta scrRam+$100,x
        lda scrData+$200,x
        sta scrRam+$200,x       
        lda scrData+$2e8,x
        sta scrRam+$2e8,x
        lda colData,x
        sta colRam,x
        lda colData+$100,x
        sta colRam+$100,x
        lda colData+$200,x
        sta colRam+$200,x
        lda colData+$2e8,x
        sta colRam+$2e8,x
        inx
        bne !loop-
        lda backGroundColor
        sta $d021

//to be added to the main program
//.import binary "pongy61.prg",2
//adresses for 	$6000 
//scrData=		$7f40
//scrRam=		$5c00
//colData=		$8328
//colRam=		$d800
// bitmap settings for $6000
        // lda #$3b		// %0011 1011           
        // sta $d011
        // lda #$18 		// %0001 1000
        // sta $d016
        // lda #$78      	// %0111 1000
        // sta $d018        
        // lda #$16       
        // sta $dd00 
 // return to text mode to default settings
        // lda #$1b             
        // sta $d011
        // lda #$08
        // sta $d016                
        // lda #$14              
        // sta $d018             
        // lda #$17                
        // sta $dd00 
}

.macro PrintByteScreen(byteaddress,xcoor,ycoor,color) {
.var textAddress = $0400+(ycoor*40)+xcoor
.var colorAddress = $d800+(ycoor*40)+xcoor

		StoreState()
		lda #color
		sta colorAddress
		sta colorAddress+1
		lda byteaddress 
		pha
                lsr
                lsr
                lsr
                lsr
		cmp #$0a
                bcs !+
		ora #$30
                bne !++
!:   		sbc #$09
!:	        sta textAddress
		pla
                and #$0f
		cmp #$0a
                bcs !+
	 	ora #$30
                bne !++
!:		sbc #$09
!:	        sta textAddress+1
		RestoreState()
}

.macro PrintChar2Screen(charnumber,screenstart,xcoor,ycoor) {
.var textAddress = screenstart+(ycoor*40)+xcoor
.var colorAddress = $d800+(ycoor*40)+xcoor

		lda #charnumber
		sta textAddress

		ldx #charnumber 	
		lda $5400,x 				
		sta colorAddress
 }

.macro PrintSameChar2Screen(charnumber,screenstart,xcoor,ycoor,number) {
.var textAddress = screenstart+(ycoor*40)+xcoor
.var colorAddress = $d800+(ycoor*40)+xcoor

		ldx #$00
		lda #charnumber
!:		sta textAddress,x 
		inx 
		cpx #number
		bne !-

		ldy #$00
		ldx #charnumber 	
		lda $5400,x 				
!:		sta colorAddress,y 
		iny
		cpy #number 
		bne !-
 }