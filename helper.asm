// Macro List:
// ClearScreen(screen,clearByte)
// ClearColorRam(clearByte)
// SetBorderColor(color)
// SetBackgroundColor(color)
// PrintScreen(message,xcoor,ycoor,color,textlength)
// PrintByteScreen(byteaddress,xcoor,ycoor,color)
// DrawScreen(screenData,colorData,screenAddress)
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

.macro ClearColorRam(clearByte) {
	lda #clearByte
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
	.var key = 0
loop:   jsr $ff9f                // waits for space key
        jsr $ffe4       
        sta key
        cmp #$20                // compares it to space ($20=32)
        bne loop 
}

.macro WaitAKey() {
loop:   jsr $ff9f                // waits for space key
        jsr $ffe4       
        beq loop 
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

.macro waitForRasterLine( line ) {
		lda #line
!loop:		cmp $d012
		bne !loop-	
}


.macro PrintScreen(message,xcoor,ycoor,color,textlength) {
	.var textAddress = $0400+(ycoor*40)+xcoor
	.var colorAddress = $d800+(ycoor*40)+xcoor
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
        // lda #$3b		           
        // sta $d011
        // lda #$18 		
        // sta $d016
        // lda #$78      	
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

