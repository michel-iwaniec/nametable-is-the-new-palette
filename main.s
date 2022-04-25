;
; Author: Michel Iwaniec <michel_iwaniec@yahoo.com>
;
; Uses the nmi_sync library to ensure minimal CPU / PPU jitter between frames, authored by Shay Green
;
; Test graphics taken from Prince of Persia title screen for NES
;
; This program rewrites the NES's palette mid-frame in hblank without incurring background glitches, something previously believed to not
; be possible due to the few CPU cycles available and the VADDR registers dual nature as VRAM pointer / scrolling state.
;
; It also demonstrates how 4 out of 8 sprites can still be displayed on the next line for certain CPU / PPU alignments and reset phases.
; The sprite part is still a work-in-progress only meant for debugging purposes. Longer-term goal is predicting / mitigating the corruption
; That takes place as a result of sprite fetch.
;
; Method:
; 
;  The NES's scrolling register VADDR (accessed via $2005 / $2006) double as the address register used for writing the palette in the PPUs memory space,
;  resulting in scrolling being corrupted by palette rewrites.
;
;  To avoid having to set the palette and restore the scroll fully, the code exploits the fact that the PPU address for the nametable at $2C00
;  will be equal to the PPU palette address space whenever coarse-Y and fine-y have certain values:
;  Y = 24-29
;  y = 3 or 7
;
;  $2C00 will be referred to as the "palette shadow nametable" (PSNT) throughout the code.
;
;  Depending on which scanline is changed, the vblank code copies a row of 32 bytes from the nametable (stored in ROM here) into
;  row 24 of $2C00, so that the palette rewrite can be performed in increments of 4 scanlines.
;
;  The palette rewrite itself needs 3 scanlines (/hblanks) to execute.
;
;  1st hblank:
;   - Does a full loopy-scroll write to the scrolling / address registers so that they point at row 24 of nametable $2C00
;   - Switches the 1-screen nametable to the "palette shadow nametable"
;   - This sets up the address so that it will increment to $3Fxx on the start of the *next* hblank
;  2nd hblank:
;   - Writes to (a-mirror-of*) $2005 *before* the hblank starts, so that VADDR points at the appropriate palette index
;   - Writes (a-mirror-of*) $2005 at start of hblank, so that the pixels output on the next scanline have the correct fine-x
;   - Writes $2001 / $2007 / $2001 to disable rendering, write the palette and re-enable the screen in as few cycles as possible.
;   - Writes $2006 to re-initialize bits 0-5 of VADDR to once again serve as coarse-scroll, just in time for fetch of two first tiles of next scanline.
;   - Performs a final write to $2001 to restore correct settings, as a previous write shared bits with the palette rewrite value
;   - The second hblank is executed in ZP RAM, to allow self-modifying code and save a cycle.
;  3rd hblank:
;    - Does a full loopy-scroll write to the scrolling / address registers so that they once again use the usual scroll coordinates
;    - Switches the 1-screen nametable to the ordinary nametable
;
;  * The mirror writes prevent temporary bus contention (and visible output artifacts), as the 6502 CPU will drive the write pin *before*
;  driving valid data onto the data bus. The open bus behavior would result in $20 (high byte of $2005) being driven for a few dots.
;  By writing to an appropriate mirror registers where bits 0-2 of the high address byte match bits 0-2 of the data, the data can be made 
;  consistent throughout the entire write cycle.
;
;  For simplicity, the mapper30 variant with 1-screen mirroring is used. However, the technique can be used with any kind of mirroring, 
;  as long as the "palette shadow nametable" can be mapped to $2C00.
;
;  Known compatibility issues:
;   
;    * Will not work correctly on the HiDef-NES mod
;      - Only BG0 will be rewritten, no matter what the controller sets the palette index to.
;    * Will not work correctly on NESRGB in RGB mode
;      - The palette rewrite will bypass the palette interception being done and actually end up in PPU palette memory that's normally filtered by NESRGB.
;      - This will cause the monochrome composite output to contain actual colors, giving checkerboard artifacts in the RGB output instead of color changes.
;      - Can still work normally on NESRGB by switching it to composite mode.
;    * Is unlikely to work on other Famiclones as well
;    * Sprite are VERY reset-phase dependent in general
;      - One fairly rare bootup state shows up to 4 stable sprites during with no other OAM corruption
;      - The expected "best" pattern is for only the 1st and last 3 sprites to disappear when intersecting the rewrite line
;
; Control scheme:
;
; Controller#1:
;   * Scroll background with cursor
;   * B + Up / Down to move the scanline where the palette rewrite is performed
;   * B + Left / Right to fine-tune the palette rewrite's alignment with hblank between 0..3 CPU cycles (always done in single-step increments)
;   * A + Left/Right to cycle which PPU palette index will be changed ($00-$1F)
;   * B + Up/Down to cycle values to write
;   * Hold START to make any of the changes above in single-step increments
;   * Press SELECT to toggle displaying grid of 8x8 sprites (affects $2001)
; Controller#2:
;   * Scroll grid of 8x8 sprites with cursor
;   * Hold START/A/B/SELECT to temporarily turn on Red/Green/Blue/Mono bit in $2001
;
; TODOs:
;   * Automatic detection of reset-dependent CPU / PPU clock phase alignment, and compensation in code
;   * Displaying current settings as readable text in some sort of status bar
;   * More controls for sprites (moving and changing individual ones)
;   * Allowing rewrite on every scanline instead of rounding to every 4th scanline, by duplicating CHR into the 4 banks
;   * Allow changing multiple colors, at the expense of fewer sprites
;   * PAL support
; To investigate:
;   * Exact pattern for OAM corruption - can it be made predictable?
;   * Try moving palette rewrite to *end* of sprite CHR fetch, for more predictable sprite corruption, at the expense of left-side BG glitches / fewer sprites
;   * For some rare reset phases, the program appears to change *two* palette entries with only a single write, with one seemingly coming from another palette entry
;
; Special thanks:
;
;   While this technique has to my knowledge not been attempted before, it would never have been possibly without the decades of reverse engineering by the amazing nesdev community.
;
; In particular:
;   - Neal Tew (Loopy), for giving us "The Skinny on NES scrolling" way back in 1999
;   - Brad Taylor, for starting the cycle-exact understanding of the PPU rendering with "NTSC 2C02 technical reference
;   - Shay Green (Blargg), for his work on consistent frame synchronization and the nmi_sync library
;   - lidnariq, for figuring out how to de-glitch mid-scanline writes to fine-x
;   - Drag, Quietust, Ulfalizer and Fiskbit for carrying on the torch of drilling deeper into the PPU rendering process
;   - Sour, for creating the most amazing NES emulator / debugger on the planet
;

.include "nmi_sync.s"
.include "delaymacro.s"

sprites                     = $200          ; OAM page
psntRow                     = $500          ; CPU memory copy of row data to copy to PSNT
psntRowAT                   = $520          ; Corresponding attribute data
INIT_FINE_DELAY = 1                         ; Initial value for HBlank fine-tune delay. Allowed range: 0 <= INIT_FINE_DELAY <= 3
SCAN_ACC_ADD    = 171                       ; NTSC: 0.666 fractional cycles / scanline -> 256 * 0.333 ~= 171
PSNT_ADDRESS    = $2C00                     ; PPU memory address for PSNT
PSNT_DST_ROW    = 24                        ; Destination row in PSNT

.zeropage
PaletteWriteHBlankCodeRAM:      .res 64         ; RAM location of core palette-write hblank code
PaletteWriteHBlankCodeRAM_END:
paletteCache:                   .res 25         ; Entire palette, excluding mirror registers
ScrollX:                        .res 1          ; Background scroll X
ScrollY:                        .res 1          ; Background scroll Y
ScrollY_t:                      .res 1          ; Background scroll Y (temporary variable incremented by raster code)
scanAcc:                        .res 1          ; Scanline accumulator used to emulate fractional cycles
numLines:                       .res 1
tmp:                            .res 2
fineDelay:                      .res 1          ; HBlank fine-tuning delay
frameCounter:                   .res 1
tmpB:                           .res 2
delayMacroVar:                  .res 1
x2001_enableOrDisableMask:      .res 1          ; $2001 render mask for reg X in HBlank code
palRewriteIndex:                .res 1          ; Palette index to rewrite mid-frame
palRewriteValue:                .res 1          ; Palette value to rewrite mid-frame
palRewriteScanline:             .res 1          ; Desired scanline to execute palette rewrite
palRewriteScanlineAdjusted:     .res 1          ; Adjusted scanline, to make sure fine-y = 3 or 7
r2001:                          .res 1          ; $2001 value
psntDstAddr:                    .res 2          ; Stores destination of where row is copied into "palette shadow nametable"
psntDstAddrAT:                  .res 2          ; Same for row's attribute data
psntSrcRow:                     .res 1          ; Row index in source (ROM) nametable
joy:
joy0:                           .res 1
joy1:                           .res 1
joyP:
joyP0:                          .res 1
joyP1:                          .res 1

JOY_A       = %00000001
JOY_B       = %00000010
JOY_SELECT  = %00000100
JOY_START   = %00001000
JOY_UP      = %00010000
JOY_DOWN    = %00100000
JOY_LEFT    = %01000000
JOY_RIGHT   = %10000000

;
; Delay by a fractional amount of cycles using the scanline accumulator byte
;
; Takes 11.666 cycles (NTSC)
;
.MACRO DELAY_ACC
    lda scanAcc
    clc
    adc #SCAN_ACC_ADD
    bcs :+
:
    sta scanAcc
.ENDMACRO

;
; Writes palette cache to PPU
;
.MACRO WRITE_PALETTE_CACHE
    lda #$3F
    sta $2006
    lda #$00
    sta $2006
    ldx paletteCache
.repeat 8,i
    stx $2007
    lda paletteCache+1+3*i+0
    sta $2007
    lda paletteCache+1+3*i+1
    sta $2007
    lda paletteCache+1+3*i+2
    sta $2007
.endrep
.ENDMACRO

;
; Sets X / Y of a single sprite
;
.MACRO SET_SPRITE spriteIndex, spriteX, spriteY
    lda spriteX
    sta sprites+spriteIndex*4+3
    lda spriteY
    sta sprites+spriteIndex*4+0
.ENDMACRO

.CODE
reset:
    sei
    ldx #0
    txa
:
    sta $000,x
    sta $100,x
    sta $200,x
    sta $300,x
    sta $400,x
    sta $500,x
    sta $600,x
    sta $700,x
    inx
    bne :-
    dex
    txs
    lda #$C0
    sta $4017
:
    lda $2002
    bpl :-

:
    lda $2002
    bpl :-
    ; Setup delay
    lda #INIT_FINE_DELAY
    sta fineDelay
    lda #$00
    sta ScrollX
    ; Copy ROM to RAM to enable self-modifying code
    jsr CopyPaletteWriteHBlankCodeToRAM
    ; Clear nametables
    jsr ClearNameTables
    ; Initialize PPU and palette
    jsr UploadCHR
    jsr UploadSpriteCHR
    jsr UploadNameTable
    lda #128
    sta palRewriteScanline
    jsr AdjustPalRewriteScanline
    jsr SetPalette
    WRITE_PALETTE_CACHE
    jsr InitOAM
    ; Disable sprites / BG in leftmost column
    lda #$18
    sta r2001
    ; Start off with changing BG0 to a bright green
    lda #$2A
    sta palRewriteValue
    lda #$00
    sta palRewriteIndex
    ; Initialise PPU addresses for PSNT
    lda #<(PSNT_ADDRESS + 32 * PSNT_DST_ROW)
    sta psntDstAddr
    lda #>(PSNT_ADDRESS + 32 * PSNT_DST_ROW)
    sta psntDstAddr+1
    lda #<(PSNT_ADDRESS + $3C0 + (PSNT_DST_ROW / 4)*8)
    sta psntDstAddrAT
    lda #>(PSNT_ADDRESS + $3C0 + (PSNT_DST_ROW / 4)*8)
    sta psntDstAddrAT+1
    lda #0
    sta ScrollY
    ; Synchronize to PPU and enable NMI
    jsr init_nmi_sync
    lda #$90
    sta $2000
    
MainLoop:
    jsr wait_nmi
    jmp MainLoop

.align 256 ; branches must not cross page
nmi:
    pha
    
    ; Do this sometime before you DMA sprites
    jsr begin_nmi_sync
    
    ; DMA then enable sprites. Instructions before
    ; STA $4014 (excluding begin_nmi_sync) must take
    ; an even number of cycles. The only required
    ; instruction here is STA $4014.
    bit <0          ; to make cycle count even
    lda #0
    sta $2003
    lda #>sprites
    sta $4014
    lda #$08
    sta $2001
    
    lda #$90
    sta $2000
    WRITE_PALETTE_CACHE
    jsr WritePSNT
    lda #0
    sta $2006
    sta $2006
    lda ScrollX
    sta $2005
    lda ScrollY
    sta $2005
    sta ScrollY_t
    DELAY 1065

    jsr end_nmi_sync
    
    ; We're now synchronized exactly to 2286 cycles
    ; after beginning of frame.
    
    lda r2001
    sta $2001

    DELAY 7
    ; Apply fine delay
    ldx fineDelay
    jsr DelayByXPlus36
    ; Delay until palette should be rewritten
    jsr DelayToAdjustedScanline
    ; Rewrite the palette (takes 2 scanlines + scroll restore repeated for fun)
    jsr RewritePaletteWithoutGlitch
    ; Update state
    jsr UpdateState
    ; Adjust palette rewrite scanline to valid positions (y = 3 or y = 7)
    jsr AdjustPalRewriteScanline
    ; Copy selected calculated nametable row into PSNT for next vblank transfer
    jsr UpdatePSNT

    pla
    rti

;
; Main raster code for glitchfree palette rewrite
;
.align $100
RewritePaletteWithoutGlitch:
    DELAY 14
    ldx ScrollX
    ldy ScrollY_t
    dey
    tya
    and #7
    clc
    adc #PSNT_DST_ROW*8
    tay
    ; #$2F / #$6F will be incremented to #$3F / #$7F when hblank starts, due to y += 1
    lda #$0F
    sta $2006
    lda R2006TabX,x
    ora R2006BTabY,y
    sty $2005
    stx $2005
    sta $2006
    ; Switch to PSNT
@bankSwitchNT1:
    ldx #$80
    stx @bankSwitchNT1+1
    DELAY 4
    DELAY_ACC
    ; Set upper byte of T to #$3F / #$7F directly to prepare for *next hblank*, and reset latch to 1st write
    lda #$0F
    sta $2006
    lda ScrollY_t
    and #7
    clc
    adc #PSNT_DST_ROW*8
    sta $2005
    ; Write coarse-X for end-of-hblank to RAM code
    lda ScrollX
    lsr
    lsr
    lsr
    sta z:<(PaletteWriteHBlankCodeRAM + (PaletteWriteHBlankCodeROM_loadCoarseX - PaletteWriteHBlankCodeROM) + 1)
    ; Preload turn-off / turn-on rendering bits
    lda r2001
    tay
    lda ScrollX
    and #7
    sta tmpB+1
    ora x2001_enableOrDisableMask
    tax
    ; Set de-glitch pattern for fine-X
    lda xFineHiByteTab,x
    sta z:<(PaletteWriteHBlankCodeRAM + (PaletteWriteHBlankCodeROM_writeCurrentFineX - PaletteWriteHBlankCodeROM) + 2)
    sta z:<(PaletteWriteHBlankCodeRAM + (PaletteWriteHBlankCodeROM_writeNextFineX - PaletteWriteHBlankCodeROM) + 2)
    ;
    lda palRewriteIndex
    asl
    asl
    asl
    ora tmpB+1
    ; Write 1 palette entry
    jmp PaletteWriteHBlankCodeRAM
BackFromPaletteWriteHBlankCode:
    DELAY_ACC

    ; Repeat 3rd restore-scroll-hblank 10 times - just to make it a bit easier to locate it in Mesen's event viewer :)
    lda #10
    sta numLines

    DELAY 6
    ; Adjust ScrollY_t to compensate for previous hblanks
    jsr IncScrollY_t
    ldy ScrollY_t
@normalLoop:
    ldy ScrollY_t
    ; Restore normal scroll coordinates / nametable
    ldx ScrollX
    lda #$20
    sta $2006
    LDA R2006TabX,X
    ORA R2006BTabY,Y
    sty $2005
    stx $2005
    sta $2006
@bankSwitchNT0:
    ldx #$00
    stx @bankSwitchNT0+1
    jsr IncScrollY_t
    DELAY 30
    DELAY_ACC
    dec numLines
    bne @normalLoop
    rts

.align $100
PaletteWriteHBlankCodeROM:
PaletteWriteHBlankCodeROM_writeCurrentFineX:
    sta $2005           ; Write old fine-X and palette index before dot 256
    bit $2002
PaletteWriteHBlankCodeROM_loadCoarseX:
    ldy #0
    lda palRewriteValue ; Load palette value
PaletteWriteHBlankCodeROM_writeNextFineX:
    stx $2005           ; Write fine-X for scrolling
PaletteWriteHBlankCodeROM_renderingDisable:
    stx $2001           ; (1) Disable rendering (after dot 256)
PaletteWriteHBlankCodeROM_writeValue:
    sta $2007           ; (4) Write palette value
    sty $2006           ; (4) Restore coarse-X for scrolling
PaletteWriteHBlankCodeROM_renderingEnable:
    sta $3801           ; (4) Enable rendering
PaletteWriteHBlankCodeROM_load2001:
    lda #$18
    sta $3801           ; (4) Fully restore all $2001 bits to correct state
    jmp BackFromPaletteWriteHBlankCode

CopyPaletteWriteHBlankCodeToRAM:
    ldx #0
:
    lda PaletteWriteHBlankCodeROM,x
    sta PaletteWriteHBlankCodeRAM,x
    inx
    cpx #(PaletteWriteHBlankCodeRAM_END - PaletteWriteHBlankCodeRAM)
    bne :-
    rts

.align $100
WritePSNT:
    ; Switch to 2nd nametable
@bankSwitchNT1:
    lda #$80
    sta @bankSwitchNT1+1
    ; Write first row
    lda psntDstAddr+1
    sta $2006
    lda psntDstAddr
    sta $2006
.repeat 32,i
    lda psntRow+i
    sta $2007
.endrep
    ; Write attribute row
    lda psntDstAddrAT+1
    sta $2006
    lda psntDstAddrAT
    sta $2006
    lda psntRowAT+0
    sta $2007
    lda psntRowAT+1
    sta $2007
    lda psntRowAT+2
    sta $2007
    lda psntRowAT+3
    sta $2007
    lda psntRowAT+4
    sta $2007
    lda psntRowAT+5
    sta $2007
    lda psntRowAT+6
    sta $2007
    lda psntRowAT+7
    sta $2007
    ; Switch back to 1st nametable
@bankSwitchNT0:
    lda #$00
    sta @bankSwitchNT0+1
    rts

UpdatePSNT:
    @oddAttributeRow = tmpB    
    lda ScrollX
    lda palRewriteIndex
; NT bytes
    lda psntSrcRow
    sta tmp
    lda #0
    asl tmp
    rol
    asl tmp
    rol
    asl tmp
    rol
    asl tmp
    rol
    asl tmp
    rol
    sta tmp+1
    lda tmp
    clc
    adc #<NameTableData
    sta tmp
    lda tmp+1
    adc #>NameTableData
    sta tmp+1
    ldx #0
:
    txa
    tay
    lda (tmp),y
    sta psntRow,x
    inx
    cpx #32
    bne :-
    lda #<(PSNT_ADDRESS + 32 * PSNT_DST_ROW)
    sta psntDstAddr
    lda #>(PSNT_ADDRESS + 32 * PSNT_DST_ROW)
    sta psntDstAddr+1
; AT bytes
    lda psntSrcRow
    lsr
    lsr
    ror @oddAttributeRow
    asl
    asl
    asl
    clc
    adc #<(NameTableData + $3C0)
    sta tmp
    lda #0
    adc #>(NameTableData + $3C0)
    sta tmp+1
    ldx #0
@copyAttributesLoop:
    txa
    tay
    lda (tmp),y
    bit @oddAttributeRow
    bpl :+
    lsr
    lsr
    lsr
    lsr
:
    sta psntRowAT,x
    inx
    cpx #8
    bne @copyAttributesLoop
    lda #<(PSNT_ADDRESS + $3C0 + (PSNT_DST_ROW / 4)*8)
    sta psntDstAddrAT
    lda #>(PSNT_ADDRESS + $3C0 + (PSNT_DST_ROW / 4)*8)
    sta psntDstAddrAT+1
    rts

UpdateRewritePosition:
    lda joy0
    and #JOY_B
    beq @dontUpdate
    lda joyP0 ;joy0,x
    and #JOY_LEFT
    beq :+
    ; Decrement with clamp-to-zero
    dec fineDelay
    bpl :+
    inc fineDelay
:
    lda joyP0 ;joy0,x
    and #JOY_RIGHT
    beq :+
    ; Increment with clamp-to-3
    inc fineDelay
    lda fineDelay
    cmp #4
    bne :+
    dec fineDelay
:
    lda joy0,x
    and #JOY_UP
    beq :+
    dec palRewriteScanline
    lda palRewriteScanline
    ; Clamp-to-4 to not crash
    cmp #4
    bcs :+
    lda #4
    sta palRewriteScanline
:
    lda joy0,x
    and #JOY_DOWN
    beq :+
    inc palRewriteScanline
    lda palRewriteScanline
    ; Clamp-to-192 to not crash
    cmp #192
    bcc :+
    lda #192
    sta palRewriteScanline
:
@dontUpdate:
    rts

;
; Handle palette rewrite index increment / decrement
;
UpdatePalRewrite:
    lda joy0
    and #JOY_A
    beq @NoPalRewriteChange
    lda joy0,x
    and #JOY_LEFT
    beq :+
    dec palRewriteIndex
:
    lda joy0,x
    and #JOY_RIGHT
    beq :+
    inc palRewriteIndex
:
    ; Limit palRewriteIndex to just the first mirror
    lda palRewriteIndex
    and #$1F
    sta palRewriteIndex
;
; Handle palette rewrite index increment / decrement
;
    lda joy0,x
    and #JOY_UP
    beq :+
    dec palRewriteValue
:
    lda joy0,x
    and #JOY_DOWN
    beq :+
    inc palRewriteValue
:
    ; Limit palRewriteValue to $00-$3F
    lda palRewriteValue
    and #$3F
    sta palRewriteValue
@NoPalRewriteChange:
    rts

UpdateScrollXY:
    lda joy0
    and #JOY_B+JOY_A
    bne @NoScrollXY
    lda joy0,x
    and #JOY_LEFT
    beq :+
    inc ScrollX
:
    lda joy0,x
    and #JOY_RIGHT
    beq :+
    dec ScrollX
:
    lda joy0,x
    and #JOY_UP
    beq @noUp
    lda ScrollY
    cmp #239
    bne :+
    clc
    adc #$10
:
    clc
    adc #1
    sta ScrollY
@noUp:
    lda joy0,x
    and #JOY_DOWN
    beq @noDown
    lda ScrollY
    bne :+
    sec
    sbc #$10
:
    sec
    sbc #1
    sta ScrollY
@noDown:
@NoScrollXY:
    rts

UpdateSpriteGrid:
    lda joyP0
    and #JOY_SELECT
    beq :+
    ; toggle showing sprites on/off
    lda r2001
    eor #%00010000
    sta r2001
:
    lda joy1,x
    and #JOY_LEFT
    beq :+
    jsr @moveSpriteGridLeft
:
    lda joy1,x
    and #JOY_RIGHT
    beq :+
    jsr @moveSpriteGridRight
:
    lda joy1,x
    and #JOY_UP
    beq :+
    jsr @moveSpriteGridUp
:
    lda joy1,x
    and #JOY_DOWN
    beq :+
    jsr @moveSpriteGridDown
:
    rts

@moveSpriteGridLeft:
.repeat 64, I
    dec sprites+4*I+3
.endrep
    rts
@moveSpriteGridRight:
.repeat 64, I
    inc sprites+4*I+3
.endrep
    rts
@moveSpriteGridUp:
.repeat 64, I
    dec sprites+4*I+0
.endrep
    rts
@moveSpriteGridDown:
.repeat 64, I
    inc sprites+4*I+0
.endrep
    rts

UpdateEmphasisBits:
    lda r2001
    and #$1E
    sta r2001
    ; Red emphasis
    lda joy1
    and #JOY_START
    beq :+
    lda r2001
    ora #%00100000
    sta r2001
:
    ; Green emphasis
    lda joy1
    and #JOY_B
    beq :+
    lda r2001
    ora #%01000000
    sta r2001
:
    ; Blue emphasis
    lda joy1
    and #JOY_A
    beq :+
    lda r2001
    ora #%10000000
    sta r2001
:
    ; Monochrome mode
    lda joy1
    and #JOY_SELECT
    beq :+
    lda r2001
    ora #%00000001
    sta r2001
:
    rts

UpdateState:
    jsr ReadJoypads
    ; If START pressed, offset by 2 to access joyP0 (single-step) for all movements
    ldx #0
    lda joy0
    and #JOY_START
    beq :+
    ldx #2
:
    jsr UpdateRewritePosition
    jsr UpdatePalRewrite
    jsr UpdateScrollXY
    jsr UpdateSpriteGrid
    jsr UpdateEmphasisBits
    jsr SetScanlineAccumulator
    jsr PatchWithAAXorXAA
    inc frameCounter
    rts

;
; Patches hblank code to either use:
;
;   sta $2001 / sta $2001 / stx $2001 (AAX)
; or:
;   stx $2001 / sta $2001 / sta $2001 (XAA)
;
; ...based on whether a write of palRewriteValue to $2001 would disable or enable rendering.
; This trick frees up a register to make the $2006 write to restore scrolling happen 2 CPU cycles earlier.
;
PatchWithAAXorXAA:
    ;
    lda r2001
    sta z:<(PaletteWriteHBlankCodeRAM + (PaletteWriteHBlankCodeROM_load2001 - PaletteWriteHBlankCodeROM + 1))
    lda palRewriteValue
    and #$18
    bne @xaa
    ; palRewriteValue would disable rendering - use AAX write pattern
    lda #$18
    sta x2001_enableOrDisableMask
    ldx #$8D ; lda_abs opcode
    txa
    ldy #$8E ; ldx_abs opcode
    jmp @patchOpcodes
@xaa:
    ; palRewriteValue would enable rendering - use XAA write pattern
    lda #$00
    sta x2001_enableOrDisableMask
    ldx #$8E ; ldx_abs opcode
    lda #$8D ; lda_abs opcode
    tay
@patchOpcodes:
    stx z:<(PaletteWriteHBlankCodeRAM + (PaletteWriteHBlankCodeROM_renderingDisable - PaletteWriteHBlankCodeROM))
    sta z:<(PaletteWriteHBlankCodeRAM + (PaletteWriteHBlankCodeROM_writeValue - PaletteWriteHBlankCodeROM))
    sty z:<(PaletteWriteHBlankCodeRAM + (PaletteWriteHBlankCodeROM_renderingEnable - PaletteWriteHBlankCodeROM))
    rts
;
; Rounds the scanline to start the palette rewrite effect, so it always takes place at y = 3 or y = 7
; Additionally sets the row to be copied based on ScrollY + rounding
;
AdjustPalRewriteScanline:
    ;
    lda ScrollY
    clc
    adc palRewriteScanline
    and #7
    ; Skip rounding if fine-y is already 3 or 7
    cmp #7
    beq :+
    cmp #3
    beq :+
    sta tmp
    ; Round to either #3 or #7
    and #4
    ora #3
    sec
    sbc tmp
    sta tmp
    clc
    adc palRewriteScanline
    sta palRewriteScanlineAdjusted
    ; Set row to copy
    lda palRewriteScanlineAdjusted
    jsr AddScrollY
    lsr
    lsr
    lsr
    sta psntSrcRow
    rts
:
    lda palRewriteScanline
    sta palRewriteScanlineAdjusted
    jsr AddScrollY
    lsr
    lsr
    lsr
    sta psntSrcRow
    rts

;
; Delay to adjusted scanlines
;
DelayToAdjustedScanline:
    ldy palRewriteScanlineAdjusted
    ; Adjust by a further 2 scanlines to compensate for 1st hblank, surrounding code and fine-tune-delay
    ; scanlines -= 3
    ; ScrollY_t += 3
    dey
    jsr IncScrollY_t
    dey
    jsr IncScrollY_t
@scanlineLoop:
    DELAY 7
    DELAY 32
    DELAY 32
    jsr IncScrollY_t
    DELAY_ACC
    dey
    bne @scanlineLoop
    DELAY 16
    rts

.align $100
;
; Delays by fineDelay cycles + 
;
DelayByXPlus36:
    cpx #0
    bne :+
    ; 0 + 36 = 36
    DELAY 12
    rts
:
    dex
    bne :+
    ; 1 + 36 = 37
    DELAY 8
    rts
:
    dex
    bne :+
    ; 2 + 36 = 38
    DELAY 4
    rts
:
    dex
    bne :+
    ; 3 + 36 = 39
    rts
:
; Should never happen unless constant out-of-range - do endless loop if so
:
    jmp :-

;
; Increment ScrollY_t with handling of 239->0 wrapping    
;
; 25 cycles
;
IncScrollY_t:
    lda ScrollY_t
    cmp #239
    beq :+
    inc a:ScrollY_t
    rts
:
    lda #0
    sta ScrollY_t
    rts

;
; Add a positive offset to ScrollY (leaving result in A), with handling of 239->0 wrapping
;
AddScrollY:
    clc
    adc ScrollY
    bcs @handleWrap
    cmp #240
    bcs @handleWrap
    rts
@handleWrap:
    clc
    adc #$10
    rts

; Freeze program if this somehow gets triggered, rather
; than silently messing up timing
irq:    jmp irq

.align 256
R2006TabX:
.repeat 256,i
.byte ((i & $FF)>>3) & $1F
.endrep

R2006BTabY:
.repeat 256,i
.byte ((i>>3)<<5) & $FF
.endrep

ClearNameTables:
    lda #$20
    sta $2006
    lda #$00
    sta $2006
@bankSwitchNT1:
    lda #$80
    sta @bankSwitchNT1+1
    jsr @clear4Nametables
@bankSwitchNT0:
    lda #$00
    sta @bankSwitchNT0+1
@clear4Nametables:
    ldy #16
    ldx #0
    lda #0
:
    sta $2007
    inx
    bne :-
    dey
    bne :-
    rts

UploadCHR:
    @ChrPtr = tmp
    lda #$10
    sta $2006
    lda #$00
    sta $2006
    lda #>TileData
    sta @ChrPtr+1
    lda #<TileData
    sta @ChrPtr
    ldx #16
    ldy #0
:
    lda (@ChrPtr),y
    iny
    sta $2007
    bne :-
    inc @ChrPtr+1
    dex
    bne :-
    rts

UploadSpriteCHR:
    @ChrPtr = tmp
    lda #$00
    sta $2006
    lda #$00
    sta $2006
    lda #>SpriteTileData
    sta @ChrPtr+1
    lda #<SpriteTileData
    sta @ChrPtr
    ;
    ldx #64
@tileLoop:
    ldy #0
@byteLoopP0:
    lda (@ChrPtr),y
    sta $2007
    iny
    cpy #8
    bne @byteLoopP0
    ldy #0
@byteLoopP1:
    lda (@ChrPtr),y
    and #$F0
    sta tmpB
    lda #$FF
    eor tmpB
    sta $2007
    iny
    cpy #8
    bne @byteLoopP1
    lda @ChrPtr
    clc
    adc #16
    sta @ChrPtr
    lda @ChrPtr+1
    adc #0
    sta @ChrPtr+1
    dex
    bne @tileLoop
    rts

UploadNameTable:
    @ChrPtr = tmp
    lda #$20
    sta $2006
    lda #$00
    sta $2006
    lda #>NameTableData
    sta @ChrPtr+1
    lda #<NameTableData
    sta @ChrPtr
    ldx #4
    ldy #0
:
    lda (@ChrPtr),y
    iny
    sta $2007
    bne :-
    inc @ChrPtr+1
    dex
    bne :-
    rts
    
InitOAM:
    ldx #0
:
    ; Y = 74 + (spriteIndex / 8) * 9
    txa
    lsr
    lsr
    lsr
    lsr
    lsr
    sta tmpB
    asl
    asl
    asl
    clc
    adc tmpB
    clc
    adc #74
    sta sprites,x
    ; Tile number = spriteIndex
    txa
    lsr
    lsr
    sta sprites+1,x
    ; Attributes = spriteIndex & 3
    txa
    lsr
    lsr
    and #3
    sta sprites+2,x
    ; X = 16*(spriteIndex & 7)) + 60
    txa
    lsr
    lsr
    and #7
    asl
    asl
    asl
    asl
    clc
    adc #60
    sta sprites+3,x
    inx
    inx
    inx
    inx
    bne :-
    rts

SetPalette:
    ldy #0
:
    lda @palette,y
    sta paletteCache,y
    iny
    cpy #25
    bne :-    
    rts
    
@palette:
.byte $1D
;
.byte $01,$13,$23
.byte $17,$27,$01
.byte $17,$27,$30
.byte $05,$15,$20
;
.byte $13,$1D,$00
.byte $16,$1D,$00
.byte $19,$1D,$00
.byte $1C,$1D,$00

ReadJoypads:
    ldx     #1
    stx     $4016
    dex
    stx     $4016
    jsr     ReadJoy
    inx
ReadJoy:
    lda     joy,X
    pha
    ldy     #8
:   lda     $4016,X
    lsr
    ror     joy,X
    dey
    bne :-
    pla
    eor joy,X
    and joy,X
    sta joyP,X
    rts

;
; Initialize scanline accumulator slightly different for odd/even frame
;
; TODO: Detect reset phase and adjust initialization
;
SetScanlineAccumulator:
    lda frameCounter
    and #1
    bne :+
    lda #0
    sta a:scanAcc
    rts
:
    lda #(256-SCAN_ACC_ADD)
    sta scanAcc
    rts

;
; Table to use for de-glitching fine-X writes performed during rendering
;
.align $100
xFineHiByteTab:
.repeat 256,i
    .byte (i & 7) | $20
.endrep

TileData:
.INCBIN "pop_extended_border.ppu", $1000, $1000

SpriteTileData:
.incbin "numbers.chr", $0000, $400

NameTableData:
.INCBIN "pop_extended_border.ppu", $2400, $400

.segment "HEADER"
    .byte "NES",26, 1,0, $E0 + %00001000, $10 ; 16K PRG, 8K CHR, Mapper30
    .byte 0,0,0,0,0,0,0,0

.segment "VECTORS"
    .word 0,0,0, nmi, reset, irq
