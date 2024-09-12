;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
;@                                                                            @
;@                                 S y M o n                                  @
;@                           SYMBOS MEMORY MONITOR                            @
;@                                                                            @
;@               (c) 2022 by Prodatron / SymbiosiS (Jörn Mika)                @
;@                                                                            @
;@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

;Bugs

;Todo
;- application details
;- clickable map



;==============================================================================
;### CODE AREA ################################################################
;==============================================================================


;### PRGPRZ -> Application process
prgwin  db 0    ;main window ID

prgprz  call SySystem_HLPINI

        call poiini
        call appget
        call mapini

        ld de,wingam
        ld a,(App_BnkNum)
        call SyDesktop_WINOPN       ;open startmenu edit window
        jp c,prgend                 ;memory full -> quit process
        ld (prgwin),a               ;window has been opened -> store ID
        call stasum

prgprz0 ld a,(stafoc)
        dec a
        jr z,prgprz3
        ld a,(stacnt)               ;only check changed memory, if not focus, as the dropdown will change the free memory during open session
        inc a
        and 31
        ld (stacnt),a
        jp z,stachk
prgprz2 ld ix,(App_PrcID)           ;check for messages (idle)
        db #dd:ld h,-1
        ld iy,App_MsgBuf
        rst #30
        rst #18
        jr prgprz4

prgprz3 ld ix,(App_PrcID)           ;check for messages (sleep)
        db #dd:ld h,-1
        ld iy,App_MsgBuf
        rst #08

prgprz4 db #dd:dec l
        jr nz,prgprz0
        ld a,(App_MsgBuf+0)
        or a
        jr z,prgend
        cp MSR_DSK_WFOCUS
        jr z,prgprz5
        cp MSR_DSK_WCLICK
        jr nz,prgprz0
        ld a,(App_MsgBuf+2)
        cp DSK_ACT_CLOSE
        jr z,prgend
        cp DSK_ACT_MENU
        jr z,prgprz1
        cp DSK_ACT_CONTENT
        jr nz,prgprz0
prgprz1 ld hl,(App_MsgBuf+8)
        ld a,h
        or h
        jr z,prgprz0
        jp (hl)
prgprz5 ld a,(App_MsgBuf+2)
        ld (stafoc),a
        dec a
        jp z,stachk
        jr prgprz0

;### PRGEND -> End program
prgend  ld hl,(App_BegCode+prgpstnum)
        call SySystem_PRGEND
prgend0 rst #30
        jr prgend0

;### PRGINF -> open info window
prginf  ld a,(App_BnkNum)
        ld hl,prgmsginf
        ld b,8*2+1+64+128
        ld de,wingam
        call SySystem_SYSWRN
        jp prgprz0

;### PRGHLP -> shows help
prghlp  call SySystem_HLPOPN
        jp prgprz0

;### PRGTAB -> tab click
prgtab  ;...
        jp prgprz0


;==============================================================================
;### SUMMARY ROUTINES #########################################################
;==============================================================================

sumops  dw 0    ;sum os
sumapp  dw 0    ;sum apps

;### SUMZER -> resets sums
sumzer  ld hl,0
        ld (sumops),hl
        ld (sumapp),hl
        ret

;### SUMADD -> adds sum to memory type
;### Input      B=length in 256byte pages, (IX)=sum value
;###Destroyed   F,D
sumadd  ld d,a
        ld a,b
        add (ix+0)
        ld (ix+0),a
        ld a,0
        adc (ix+1)
        ld (ix+1),a
        ld a,d
        ret

stafoc  db 1
stacnt  db 0
stafre  dw 0

;### STACHK -> check, if status changed
stachk  ld hl,jmp_memsum:rst #28
        ld b,e
        ld c,ixh
        ld hl,(stafre)
        or a
        sbc hl,bc
        jp z,prgprz2
        jp mapref

;### STAWAI -> shows calculation status
stawai  ld hl,winstawai
        ld (wingam+32),hl
        ld a,(prgwin)
        jp SyDesktop_WINSTA

;### STASUM -> generates and updates status bar
sumtxt
db "Total ",0
db "Free ",0
db "System ",0
db "Apps ",0

stasum  ld hl,jmp_memsum:rst #28
        ld b,e
        ld c,ixh
        ld (stafre),bc
        push bc
        ld ix,sumtxt
        ld iy,winstatxt
        inc d
        ld b,d
        ld c,0
        ld e,4
        call stasum1
        pop bc
        ld e,3
        call stasum1
        ld bc,(sumops)
        ld e,3
        call stasum1
        ld bc,(sumapp)
        ld e,3
        call stasum1
        ld (iy-2),0
        ld hl,winstatxt
        ld (wingam+32),hl
        ld a,(prgwin)
        jp SyDesktop_WINSTA
;ix=title, iy=status line, bc=256b pages -> ix,iy=updated
stasum1 ld a,(ix+0)
        inc ix
        or a
        jr z,stasum2
        ld (iy+0),a
        inc iy
        jr stasum1
stasum2 srl b:rr c
        srl b:rr c
        push bc:ex (sp),ix  ;IX=Wert
        ld hl,jmp_clcnum:rst #28
        ld (iy+1),"K"
        ld (iy+2),","
        ld (iy+3)," "
        ld bc,4
        add iy,bc
        pop ix
        ret


;==============================================================================
;### APPLICATION ROUTINES #####################################################
;==============================================================================

;### APPGID -> get selected appID
;### Ouput      A,(appgidl)=ID or -1 (none)
appgidl db -1

appgid  ld a,(appselobj+12)
        add a:add a
        ld hl,appsellst
        ld c,a
        ld b,0
        add hl,bc
        ld a,(hl)                   ;a=selected appID
        ld (appgidl),a
        ret

;### APPFND -> find entry with ID and preselects it
appfnd  ld hl,appsellst
        ld a,(appgidl)
        ld de,4
        ld bc,(appselobj-1)
        ld c,0
appfnd1 cp (hl)
        jr z,appfnd2
        add hl,de
        inc c
        djnz appfnd1
        ld c,0
appfnd2 xor a
        ld (appselobj+2),a
        ld a,c
        ld (appselobj+12),a
        ret
        
;### APPGET -> reads all applications and adds them to list
appgetb ds 90

appget  ld hl,(poiapp)
        ld de,apptxtnam
        ld iy,appsellst+4
        ld ix,256+24    ;ixl=loop, ixh=entries ("none", >=1)
appget1 xor a
        rst #20:dw jmp_bnkrbt     ;b=0/bank
        push bc
        rst #20:dw jmp_bnkrwd     ;bc=adr
        inc hl
        ex (sp),hl      ;h=bank
        inc h:dec h
        jr z,appget2
        call bnkdst
        or h
        push de
        ld l,c
        ld h,b
        ld de,appgetb
        ld bc,90
        rst #20:dw jmp_bnkcop
        ld a,24
        sub ixl
        ld (iy+0),a
        pop de
        ld (iy+2),e
        ld (iy+3),d
        ld hl,appgetb+prgdatnam
        ld bc,19
        ldir
        inc de
        ld bc,4
        add iy,bc
        inc ixh
appget2 pop hl
        dec ixl
        jr nz,appget1
        ld a,ixh
        ld (appselobj+0),a
        xor a
        ld (appselobj+2),a
        ld (appselobj+12),a
        ret


;==============================================================================
;### MEMORY MAP BITMAP ROUTINES ###############################################
;==============================================================================

maptypmsk   db %00110011,%11000000,%11001100,%00110000

maptyp_fre  equ 0
maptyp_unk  equ 1
maptyp_sel  equ 2
maptyp_app  equ 3
maptyp_vid  equ 5
maptyp_ops  equ 7

mb0_fre equ %00000000:mb1_fre equ %00000000:mb2_fre equ %00000000:mb3_fre equ %00000000     ;w
mb0_unk equ %11000000:mb1_unk equ %00110000:mb2_unk equ %11000000:mb3_unk equ %00110000     ;s
mb0_sel equ %00001100:mb1_sel equ %00000011:mb2_sel equ %00001100:mb3_sel equ %00000011     ;g
mb0_app equ %11001100:mb1_app equ %00110011:mb2_app equ %11001100:mb3_app equ %00110011     ;b
mb0_xxx equ %11001000:mb1_xxx equ %00110010:mb2_xxx equ %11000100:mb3_xxx equ %00110001     ;sb
mb0_vid equ %10000000:mb1_vid equ %00100000:mb2_vid equ %01000000:mb3_vid equ %00010000     ;sw
mb0_yyy equ %01001000:mb1_yyy equ %00010010:mb2_yyy equ %10000100:mb3_yyy equ %00100001     ;sg
mb0_ops equ %01001100:mb1_ops equ %00010011:mb2_ops equ %10001100:mb3_ops equ %00100011     ;gb

maptyptab
db mb0_fre,mb1_fre, mb0_fre,mb1_fre, mb2_fre,mb3_fre, mb2_fre,mb3_fre
db mb0_unk,mb1_unk, mb0_unk,mb1_unk, mb2_unk,mb3_unk, mb2_unk,mb3_unk
db mb0_sel,mb1_sel, mb0_sel,mb1_sel, mb2_sel,mb3_sel, mb2_sel,mb3_sel
db mb0_app,mb1_app, mb0_app,mb1_app, mb2_app,mb3_app, mb2_app,mb3_app
db mb0_xxx,mb1_xxx, mb0_xxx,mb1_xxx, mb2_xxx,mb3_xxx, mb2_xxx,mb3_xxx
db mb0_vid,mb1_vid, mb0_vid,mb1_vid, mb2_vid,mb3_vid, mb2_vid,mb3_vid
db mb0_yyy,mb1_yyy, mb0_yyy,mb1_yyy, mb2_yyy,mb3_yyy, mb2_yyy,mb3_yyy
db mb0_ops,mb1_ops, mb0_ops,mb1_ops, mb2_ops,mb3_ops, mb2_ops,mb3_ops

maptypnot   ;no memory pattern
db %10000000,%10000000
db %01000000,%01000000
db %00100000,%00100000
db %00010000,%00010000
db %10000000,%10000000
db %01000000,%01000000
db %00100000,%00100000
db %00010000,%00010000

memmap      ds 480

;### MAPREF -> updates memory map
mapref  call stawai
        call appgid
        call appget
        call appfnd
        push af
        call mapini
        ld de,256*wingam_map+256-16
        ld a,(prgwin)
        call SyDesktop_WINDIN
        ld e,wingam_sel
mapref1 ld a,(prgwin)
        call SyDesktop_WINDIN
        call stasum
        jp prgprz0

;### MAPSEL -> change selection
mapsel  call stawai
        call mapini
        ld de,256*wingam_map+256-16
        jr mapref1

;### MAPINI -> inits memory map
mapini  call sumzer
        call bnkdst
        ld hl,(poimem)
        ld de,memmap
        push de
        ld bc,480
        rst #20:dw jmp_bnkcop
        pop ix              ;ix=memmap
        ld hl,memmapg02-2   ;hl=bitmap
        ld iyl,15           ;iyl=bank counter
mapini4 ld iyh,32           ;iyh=page counter
        ld c,0              ;c=page in block
mapini5 ld e,(ix+0)         ;e=page bits
        inc ix
        ld b,2              ;b=nibble counter
mapini6 call mapinia
        call mapinia
        inc hl
        call mapinia
        call mapinia
        dec hl:dec hl:dec hl
        djnz mapini6
        bit 6,c
        jr z,mapini7
        res 6,c
        dec hl:dec hl
mapini7 dec iyh
        jr nz,mapini5
        ld bc,32*4+8+128+9
        add hl,bc
        dec iyl
        jr nz,mapini4

        xor a
        ld e,maptyp_ops
        ld bc,256*192+0         ;all -> operating system at 0,#0000,#c000
        ld ix,sumops
        call sumadd
        call mapfil
        ld a,(cfgcpctyp)        ;bit[0-4] 0-4=CPC, 6=EP, 7-10=MSX, 12-13=PCW, 15-17=NC
        and 31
        ld e,maptyp_vid         ;ep,cpc,pcw,nc -> video ram at 0,#c000,#4000    ##!!## missing vram on pcw
        cp 7
        ccf
        jr nc,mapinid
        cp 10+1
        jr nc,mapinid
        ld e,maptyp_ops         ;msx -> operating system at 0,#c000,#4000
mapinid push de
        ld a,0
        ld bc,256*64+192
        ld ix,sumops
        call c,sumadd
        call mapfil
        pop de

        ld a,maptyp_ops
        cp e
        jr z,mapinig
        ld a,1
        ld e,maptyp_ops         ;all but not msx -> operating system at 1,#0400,#3c00
        ld bc,256*60+4
        ld ix,sumops
        call sumadd
        call mapfil

mapinig ld hl,(poiops)
        xor a
        rst #20:dw jmp_bnkrwd   ;c=data len (at #4000[others]/#6000[pcw]), b=trns len (at trns ofs)
        push bc
        rst #20:dw jmp_bnkrbt   ;b=trns ofs
        ld a,b
        ld (mapinic+1),a
        pop bc
        push bc
        ld a,(cfgcpctyp)
        and 31
        ld b,c
        ld c,64
        cp 12
        jr c,mapinih
        cp 13+1
        jr nc,mapinih
        push bc
        ld a,1                  ;pcw -> video ram at 1,#4000,#2000
        ld e,maptyp_vid
        ld bc,256*32+64
        call mapfil
        pop bc
        ld c,96
mapinih ld a,1
        ld e,maptyp_ops         ;all -> operating system at 1,#4000/#6000,x
        ld ix,sumops
        call sumadd
        inc b:dec b
        call nz,mapfil
        pop bc
        ld a,1
        ld e,maptyp_ops         ;all -> operating system at 1,trns,x
mapinic ld c,0
        ld ix,sumops
        call sumadd
        inc b:dec b
        call nz,mapfil

        call mapapp

        ld a,15                 ;mark kernel area in extended banks
mapinib push af
        ld e,maptyp_ops
        ld bc,256*4+0
        ld ix,sumops
        call sumadd
        call mapfil
        pop af
        push af
        ld e,maptyp_ops
        ld bc,256*1+255
        ld ix,sumops
        call sumadd
        call mapfil
        pop af
        dec a
        jr nz,mapinib

        ld hl,(poiops)
        inc hl:inc hl:inc hl
        xor a
        rst #20:dw jmp_bnkrwd   ;bc=bankflgs
        ld l,c
        ld h,b
        add hl,hl
        add hl,hl               ;skip first 2 banks (always available)
        ld de,memmapg02+3       ;mark unavailable 64k banks
        ld b,14
mapini1 push bc
        add hl,hl
        push hl
        push de
        jr c,mapinie
        ld a,4
mapini3 call mapini2
        call mapini2
        inc de:inc de
        dec a
        jr nz,mapini3
        ld hl,(sumops)
        ld de,-5
        add hl,de
        ld (sumops),hl
mapinie pop hl
        ld bc,16*2*4+9
        add hl,bc
        ex de,hl
        pop hl
        pop bc
        djnz mapini1

        ld ix,memmapg00             ;set always back to cpc encoding
        ld b,16
        ld de,16*2*4+9
mapinif ld (ix+0),2
        ld (ix+1),8
        ld (ix+2),67
        add ix,de
        djnz mapinif
        ret

mapini2 ld hl,maptypnot
        ld bc,16
        ldir
        ret

mapinia bit 0,c
        ld a,(maptypmsk+1)  ;choose left/right masks
        ld d,a
        ld a,(maptypmsk+0)
        jr z,mapini8
        ld a,(maptypmsk+3)
        ld d,a
        ld a,(maptypmsk+2)
mapini8 and (hl)
        rrc e
        jr nc,mapini9       ;(not) set pixels
        or d
mapini9 ld (hl),a
        inc c
        ret

;### MAPAPP -> mark app memory
mapapp  call appgid                 ;a=selected appID
        ld (mapapp3+1),a
        ld a,(appselobj)
        dec a
        ld ixl,a                    ;ixl=counter
        ld iy,appsellst+4           ;(iy)=appIDs
mapapp1 ld a,(iy+0)
        ld (mapapp4+1),a            ;store this appID
        add a:add a
        ld c,a
        ld b,0
        ld hl,(poiapp)
        add hl,bc
        xor a
        rst #20:dw jmp_bnkrbt       ;b=0/bank
        push bc
        rst #20:dw jmp_bnkrwd       ;bc=adr
        ld l,c
        ld h,b
        pop af
        or a
        jr z,mapapp2
        ld (mapapp5+1),a            ;store bank
        push hl                     ;store address
        ld b,a
        call bnkdst
        or b
        ld de,appgetb
        ld bc,90
        rst #20:dw jmp_bnkcop
        pop bc
        push ix
        push iy
        ld hl,(appgetb+prgdatcod)   ;mark code area
        call mapapp3
        ld bc,(appgetb+6)           ;mark data area
        ld hl,(appgetb+prgdatdat)
        call mapapp3
        ld bc,(appgetb+8)           ;mark transfer area
        ld hl,(appgetb+prgdattra)
        call mapapp3
        ld ix,appgetb+prgpstmem     ;mark additional areas
        ld b,8
mapapp8 ld a,(ix+0)
        or a
        jr z,mapapp9
        push bc
        push ix
        ld c,(ix+1)
        ld b,(ix+2)
        ld l,(ix+3)
        ld h,(ix+4)
        call mapapp7
        pop ix
        pop bc
mapapp9 ld de,5
        add ix,de
        djnz mapapp8
        pop iy
        pop ix
mapapp2 ld bc,4
        add iy,bc
        dec ixl
        jp nz,mapapp1
        ret

mapapp7 ld (mapapp5+1),a    ;a=bank
mapapp3 ld a,0              ;bc=start, hl=length
mapapp4 cp 0
        ld e,maptyp_app
        jr nz,mapapp5
        ld e,maptyp_sel
mapapp5 ld a,0
        inc l:dec l
        jr z,mapapp6
        inc h
mapapp6 ld c,b
        ld b,h
        ld ix,sumapp
        call sumadd
        jr mapfil

;### MAPFIL -> fills map area with memory type
;### Input      A=bank (0-15), C=start page, B=length, E=type (0=free, 1=app, 2=unknown, 3=selected, 4=os, 5=vram)
mapfil  push de
        ld de,16*2*4+9
        call clcm16
        ld de,memmapg01-2+34
        add hl,de
        ld a,c
        ld de,-34
mapfil1 add hl,de           ;hl=block start
        sub 64
        jr nc,mapfil1
        add 64
        ld c,a              ;c=mem page in block (0-63)
        srl a
        ld e,a
        ld d,0
        and 1
        jr z,mapfil7
        inc hl:inc hl
mapfil7 sbc hl,de           ;hl=bitmap adr within block

        pop de
        sla e:sla e:sla e
        ld d,0
        ld ix,maptyptab
        add ix,de           ;ix=map type bitmap masks
mapfil2 ld a,c
        and 7
        ld (mapfil4+2),a
        rra
        ld a,(maptypmsk+0)
        jr nc,mapfil3
        ld a,(maptypmsk+2)
mapfil3 and (hl)
mapfil4 or (ix+0)
        ld (hl),a
        inc c
        bit 0,c
        jr nz,mapfil5
        inc hl
        bit 1,c
        jr nz,mapfil5
        ld de,-4
        add hl,de
mapfil5 bit 6,c
        jr z,mapfil6
        res 6,c
        dec hl:dec hl
mapfil6 djnz mapfil2
        ret


;==============================================================================
;### SUB-ROUTINES #############################################################
;==============================================================================

symextpgm   equ 9
symextmem   equ 15

poimem  dw 0
poiapp  dw 0
poiops  dw 0

;### POIINI -> gets memory map and application data pointers
poiini  ld e,7
        ld hl,jmp_sysinf
        rst #28                 ;IYL=Databank
        ld a,iyl
        push af
        ld e,8
        ld hl,jmp_sysinf
        rst #28
        pop af
        push iy:pop hl
        ld de,32
        add hl,de               ;hl=pointer to desktop extended vars
        rst #20:dw jmp_bnkrwd   ;bc=desktop extvars adr
        ld hl,symextmem
        add hl,bc
        xor a
        rst #20:dw jmp_bnkrwd   ;bc=memmap
        ld (poimem),bc
        ld (poiops),hl
        ld bc,symextpgm-symextmem-2
        add hl,bc
        rst #20:dw jmp_bnkrwd   ;bc=appmem
        ld (poiapp),bc
        ld hl,jmp_sysinf        ;get computer type
        ld de,256*1+5
        ld ix,cfgcpctyp
        ld iy,66+2+6+8
        rst #28
        ret

;### BNKDST -> get our bank as destination
;### Output     A[4-7]=bank, A[0-3]=0
bnkdst  ld a,(App_BnkNum)
        add a:add a:add a:add a
        ret

;### CLCM16 -> Multipliziert zwei Werte (16bit)
;### Eingabe    A=Wert1, DE=Wert2
;### Ausgabe    HL=Wert1*Wert2 (16bit)
;### Veraendert AF,DE
clcm16  ld hl,0
clcm161 or a
        ret z
        rra
        jr nc,clcm162
        add hl,de
clcm162 sla e
        rl d
        jr clcm161


;==============================================================================
;### DATA AREA ################################################################
;==============================================================================

App_BegData

prgicn16c db 12,24,24:dw $+7:dw $+4,12*24:db 5
db #FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#11,#11,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#11,#66,#66,#1F,#FF,#FF
db #FF,#FF,#FF,#FF,#FF,#11,#66,#88,#88,#61,#FF,#FF,#FF,#FF,#FF,#FF,#11,#66,#88,#88,#88,#86,#1F,#FF,#FF,#FF,#FF,#11,#66,#88,#85,#88,#88,#88,#61,#FF,#FF,#FF,#11,#66,#88,#85,#55,#58,#88,#88,#86,#1F
db #FF,#11,#66,#88,#85,#F5,#5F,#F5,#88,#88,#88,#61,#F1,#66,#88,#85,#55,#FF,#5F,#55,#58,#88,#86,#61,#16,#88,#88,#88,#55,#F5,#F5,#55,#88,#86,#61,#61,#16,#88,#88,#88,#85,#F5,#55,#88,#86,#66,#1C,#11
db #16,#81,#11,#88,#88,#55,#88,#86,#61,#66,#16,#11,#16,#61,#00,#18,#88,#88,#86,#66,#1C,#16,#16,#1F,#16,#61,#61,#88,#88,#86,#61,#66,#16,#11,#17,#1F,#F1,#61,#61,#88,#86,#66,#1C,#16,#16,#1F,#1D,#1F
db #FF,#11,#01,#86,#61,#66,#16,#11,#17,#1F,#F1,#FF,#FF,#FF,#11,#86,#1C,#16,#16,#1F,#1D,#1F,#FF,#FF,#FF,#FF,#F1,#86,#16,#11,#17,#1F,#F1,#FF,#FF,#FF,#FF,#FF,#F1,#66,#16,#1F,#1D,#1F,#FF,#FF,#FF,#FF
db #FF,#FF,#FF,#11,#17,#1F,#F1,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#1D,#1F,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#F1,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF,#FF

linsep  equ #ffff

;memory map bitmaps
memmapg00   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg01   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg02   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg03   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg04   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg05   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg06   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg07   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg08   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg09   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg10   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg11   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg12   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg13   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg14   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16
memmapg15   db 2,8,67:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16:dw linsep:ds 2*16

;legend bitmaps
leggfxfre db 2,8,8
db mb2_fre+mb3_fre, mb2_fre+mb3_fre, mb0_fre+mb1_fre, mb0_fre+mb1_fre, mb2_fre+mb3_fre, mb2_fre+mb3_fre, mb0_fre+mb1_fre, mb0_fre+mb1_fre
db mb2_fre+mb3_fre, mb2_fre+mb3_fre, mb0_fre+mb1_fre, mb0_fre+mb1_fre, mb2_fre+mb3_fre, mb2_fre+mb3_fre, mb0_fre+mb1_fre, mb0_fre+mb1_fre
leggfxunk db 2,8,8
db mb2_unk+mb3_unk, mb2_unk+mb3_unk, mb0_unk+mb1_unk, mb0_unk+mb1_unk, mb2_unk+mb3_unk, mb2_unk+mb3_unk, mb0_unk+mb1_unk, mb0_unk+mb1_unk
db mb2_unk+mb3_unk, mb2_unk+mb3_unk, mb0_unk+mb1_unk, mb0_unk+mb1_unk, mb2_unk+mb3_unk, mb2_unk+mb3_unk, mb0_unk+mb1_unk, mb0_unk+mb1_unk
leggfxsel db 2,8,8
db mb2_sel+mb3_sel, mb2_sel+mb3_sel, mb0_sel+mb1_sel, mb0_sel+mb1_sel, mb2_sel+mb3_sel, mb2_sel+mb3_sel, mb0_sel+mb1_sel, mb0_sel+mb1_sel
db mb2_sel+mb3_sel, mb2_sel+mb3_sel, mb0_sel+mb1_sel, mb0_sel+mb1_sel, mb2_sel+mb3_sel, mb2_sel+mb3_sel, mb0_sel+mb1_sel, mb0_sel+mb1_sel
leggfxapp db 2,8,8
db mb2_app+mb3_app, mb2_app+mb3_app, mb0_app+mb1_app, mb0_app+mb1_app, mb2_app+mb3_app, mb2_app+mb3_app, mb0_app+mb1_app, mb0_app+mb1_app
db mb2_app+mb3_app, mb2_app+mb3_app, mb0_app+mb1_app, mb0_app+mb1_app, mb2_app+mb3_app, mb2_app+mb3_app, mb0_app+mb1_app, mb0_app+mb1_app
leggfxvid db 2,8,8
db mb2_vid+mb3_vid, mb2_vid+mb3_vid, mb0_vid+mb1_vid, mb0_vid+mb1_vid, mb2_vid+mb3_vid, mb2_vid+mb3_vid, mb0_vid+mb1_vid, mb0_vid+mb1_vid
db mb2_vid+mb3_vid, mb2_vid+mb3_vid, mb0_vid+mb1_vid, mb0_vid+mb1_vid, mb2_vid+mb3_vid, mb2_vid+mb3_vid, mb0_vid+mb1_vid, mb0_vid+mb1_vid
leggfxops db 2,8,8
db mb2_ops+mb3_ops, mb2_ops+mb3_ops, mb0_ops+mb1_ops, mb0_ops+mb1_ops, mb2_ops+mb3_ops, mb2_ops+mb3_ops, mb0_ops+mb1_ops, mb0_ops+mb1_ops
db mb2_ops+mb3_ops, mb2_ops+mb3_ops, mb0_ops+mb1_ops, mb0_ops+mb1_ops, mb2_ops+mb3_ops, mb2_ops+mb3_ops, mb0_ops+mb1_ops, mb0_ops+mb1_ops
leggfxnot db 2,8,8
db %10000000,%10000000
db %01000000,%01000000
db %00100000,%00100000
db %00010000,%00010000
db %10000000,%10000000
db %01000000,%01000000
db %00100000,%00100000
db %00010000,%00010000

;### STRINGS ##################################################################

wintittxt   db "SyMon - SymbOS Memory Monitor",0
winstawai   db "Calculating...",0

prgwinmentx1 db "File",0
prgwinmen1tx0 db "Quit",0

tabmaitxt1  db "Memory map",0
txtbutref   db "Refresh",0

;### applications
apptxtnon   db "[none]",0
apptxtnam   ds 24*20

;### memory map
membnkt00   db "0",0
membnkt01   db "1",0
membnkt02   db "2",0
membnkt03   db "3",0
membnkt04   db "4",0
membnkt05   db "5",0
membnkt06   db "6",0
membnkt07   db "7",0
membnkt08   db "8",0
membnkt09   db "9",0
membnkt10   db "A",0
membnkt11   db "B",0
membnkt12   db "C",0
membnkt13   db "D",0
membnkt14   db "E",0
membnkt15   db "F",0

memblkt00   db "#0000",0
memblkt40   db "#4000",0
memblkt80   db "#8000",0
memblktC0   db "#C000",0
memblktFF   db "#FFFF",0

legtxtfre   db "Free",0
legtxtunk   db "Unknown",0
legtxtvid   db "VRAM",0
legtxtnot   db "N/A",0
legtxtops   db "System",0
legtxtapp   db "Applications",0
legtxtsel   db "Selected",0

;### info
prgmsginf1 db "SyMon",0
prgmsginf2 db " Version 1.0 (Build "
read "..\..\..\SVN-Main\trunk\build.asm"
           db "pdt)",0
prgmsginf3 db " Copyright <c> 2022 SymbiosiS",0



;==============================================================================
;### TRANSFER AREA ############################################################
;==============================================================================

App_BegTrns

;### PRGPRZS -> Stack for application process
        ds 128
prgstk  ds 6*2
        dw prgprz
App_PrcID  db 0
App_MsgBuf ds 14

;### INFO-FENSTER #############################################################

prgmsginf  dw prgmsginf1,4*1+2,prgmsginf2,4*1+2,prgmsginf3,4*1+2,prgicnbig


;### MAIN WINDOW ##############################################################

prgwinmen  dw 1, 1+4,prgwinmentx1,prgwinmen1,0
prgwinmen1 dw 1, 1,prgwinmen1tx0,prgend,0

wingam       dw #7501,0,63,3,209,144,0,0,209,144,209,144,209,144,prgicnsml,wintittxt,winstatxt,prgwinmen,wingamgrp,0,0:ds 136+14

wingamgrp    db 86,0: dw wingamdat1,0,0,0,0,0,0
wingamdat1
dw      0,255*256+ 0, 2,                  0,  0,225,160,0
dw prgtab,255*256+20, tabmaiobj,          0,  1,209, 11,0

dw      0,255*256+ 1, membnko00,     42+000, 14, 10,  8,0
dw      0,255*256+ 1, membnko01,     42+010, 14, 10,  8,0
dw      0,255*256+ 1, membnko02,     42+020, 14, 10,  8,0
dw      0,255*256+ 1, membnko03,     42+030, 14, 10,  8,0
dw      0,255*256+ 1, membnko04,     42+040, 14, 10,  8,0
dw      0,255*256+ 1, membnko05,     42+050, 14, 10,  8,0
dw      0,255*256+ 1, membnko06,     42+060, 14, 10,  8,0
dw      0,255*256+ 1, membnko07,     42+070, 14, 10,  8,0
dw      0,255*256+ 1, membnko08,     42+080, 14, 10,  8,0
dw      0,255*256+ 1, membnko09,     42+090, 14, 10,  8,0
dw      0,255*256+ 1, membnko10,     42+100, 14, 10,  8,0
dw      0,255*256+ 1, membnko11,     42+110, 14, 10,  8,0
dw      0,255*256+ 1, membnko12,     42+120, 14, 10,  8,0
dw      0,255*256+ 1, membnko13,     42+130, 14, 10,  8,0
dw      0,255*256+ 1, membnko14,     42+140, 14, 10,  8,0
dw      0,255*256+ 1, membnko15,     42+150, 14, 10,  8,0

dw      0,255*256+ 1, memblkoFF,          8, 19, 32,  8,0
dw      0,255*256+ 1, memblkoC0,          8, 36, 32,  8,0
dw      0,255*256+ 1, memblko80,          8, 53, 32,  8,0
dw      0,255*256+ 1, memblko40,          8, 70, 32,  8,0
dw      0,255*256+ 1, memblko00,          8, 87, 32,  8,0

dw      0,255*256+ 2, 0+4,           41+000, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+010, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+020, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+030, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+040, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+050, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+060, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+070, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+080, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+090, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+100, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+110, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+120, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+130, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+140, 23, 10, 69,0
dw      0,255*256+ 2, 0+4,           41+150, 23, 10, 69,0

dw      0,255*256+ 0, 3  ,               41, 40,160,  1,0
dw      0,255*256+ 0, 3  ,               41, 57,160,  1,0
dw      0,255*256+ 0, 3  ,               41, 74,160,  1,0

dw      0,255*256+ 0, 3  ,               36, 23,  4,  1,0
dw      0,255*256+ 0, 3  ,               36, 40,  4,  1,0
dw      0,255*256+ 0, 3  ,               36, 57,  4,  1,0
dw      0,255*256+ 0, 3  ,               36, 74,  4,  1,0
dw      0,255*256+ 0, 3  ,               36, 91,  4,  1,0

wingam_map equ 47
dw      0,255*256+ 8, memmapg00,     42+000, 24,  8, 67,0
dw      0,255*256+ 8, memmapg01,     42+010, 24,  8, 67,0
dw      0,255*256+ 8, memmapg02,     42+020, 24,  8, 67,0
dw      0,255*256+ 8, memmapg03,     42+030, 24,  8, 67,0
dw      0,255*256+ 8, memmapg04,     42+040, 24,  8, 67,0
dw      0,255*256+ 8, memmapg05,     42+050, 24,  8, 67,0
dw      0,255*256+ 8, memmapg06,     42+060, 24,  8, 67,0
dw      0,255*256+ 8, memmapg07,     42+070, 24,  8, 67,0
dw      0,255*256+ 8, memmapg08,     42+080, 24,  8, 67,0
dw      0,255*256+ 8, memmapg09,     42+090, 24,  8, 67,0
dw      0,255*256+ 8, memmapg10,     42+100, 24,  8, 67,0
dw      0,255*256+ 8, memmapg11,     42+110, 24,  8, 67,0
dw      0,255*256+ 8, memmapg12,     42+120, 24,  8, 67,0
dw      0,255*256+ 8, memmapg13,     42+130, 24,  8, 67,0
dw      0,255*256+ 8, memmapg14,     42+140, 24,  8, 67,0
dw      0,255*256+ 8, memmapg15,     42+150, 24,  8, 67,0

dw      0,255*256+ 8, leggfxfre,          9, 99,  8,  8,0
dw      0,255*256+ 2, 0+4,                8, 98, 10, 10,0
dw      0,255*256+ 1, legobjfre,         21, 99, 24,  8,0

dw      0,255*256+ 8, leggfxvid,         49, 99,  8,  8,0
dw      0,255*256+ 2, 0+4,               48, 98, 10, 10,0
dw      0,255*256+ 1, legobjvid,         61, 99, 24,  8,0

dw      0,255*256+ 8, leggfxops,         92, 99,  8,  8,0
dw      0,255*256+ 2, 0+4,               91, 98, 10, 10,0
dw      0,255*256+ 1, legobjops,        104, 99, 36,  8,0

dw      0,255*256+ 8, leggfxnot,        141, 99,  8,  8,0
dw      0,255*256+ 2, 0+4,              140, 98, 10, 10,0
dw      0,255*256+ 1, legobjnot,        153, 99, 18,  8,0

dw      0,255*256+ 8, leggfxunk,          9,111,  8,  8,0
dw      0,255*256+ 2, 0+4,                8,110, 10, 10,0
dw      0,255*256+ 1, legobjunk,         21,111, 42,  8,0

dw      0,255*256+ 8, leggfxapp,         66,111,  8,  8,0
dw      0,255*256+ 2, 0+4,               65,110, 10, 10,0
dw      0,255*256+ 1, legobjapp,         78,111, 66,  8,0

dw      0,255*256+ 8, leggfxsel,        136,111,  8,  8,0
dw      0,255*256+ 2, 0+4,              135,110, 10, 10,0
dw      0,255*256+ 1, legobjsel,        148,111, 48,  8,0

wingam_sel equ 84
dw mapsel,255*256+42,appselobj,           8,130, 92, 10,0       ;app selection
dw mapref,255*256+16,txtbutref,         151,129, 50, 12,0       ;refresh

membnko00   dw membnkt00,256*67+2+4
membnko01   dw membnkt01,256*67+2+4
membnko02   dw membnkt02,256*67+2+4
membnko03   dw membnkt03,256*67+2+4
membnko04   dw membnkt04,256*67+2+4
membnko05   dw membnkt05,256*67+2+4
membnko06   dw membnkt06,256*67+2+4
membnko07   dw membnkt07,256*67+2+4
membnko08   dw membnkt08,256*67+2+4
membnko09   dw membnkt09,256*67+2+4
membnko10   dw membnkt10,256*67+2+4
membnko11   dw membnkt11,256*67+2+4
membnko12   dw membnkt12,256*67+2+4
membnko13   dw membnkt13,256*67+2+4
membnko14   dw membnkt14,256*67+2+4
membnko15   dw membnkt15,256*67+2+4

memblko00   dw memblkt00,256*64+2+4
memblko40   dw memblkt40,256*64+2+4
memblko80   dw memblkt80,256*64+2+4
memblkoC0   dw memblktC0,256*64+2+4
memblkoFF   dw memblktFF,256*64+2+4

legobjfre   dw legtxtfre,256*64+2+4
legobjunk   dw legtxtunk,256*64+2+4
legobjvid   dw legtxtvid,256*64+2+4
legobjops   dw legtxtops,256*64+2+4
legobjapp   dw legtxtapp,256*64+2+4
legobjsel   dw legtxtsel,256*64+2+4
legobjnot   dw legtxtnot,256*64+2+4


appselobj   dw 1,00,appsellst,0,1,appselrow,0,1
appselrow   dw 0,100
appsellst
dw 255,apptxtnon
ds 24*4

tabmaiobj  db 1,2+4+48+64
tabmaiobj0 db 0:dw tabmaitxt1:db -1

;place in transarea
winstatxt   db "Total 0000K, Free 000K, System 000K, Apps 000K",0:db 0
cfgcpctyp   db 0    ;bit[0-4] 0-4=CPC, 6=EP, 7-10=MSX, 12-13=PCW

prgtrnend
