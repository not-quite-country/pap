;; PAP 1.3 - decode lots of different things
;; obviously incomplete real mode 8086 disassembler
;;
;; moved to a more explicit table-driven approach


format pe64 console
entry start

include 'win64w.inc'

section '.text' code readable executable

;; TABLES ======================================================================

;; strings ---------------------------------------------------------------------

;; construct a counted string table
;;
;; 00  tab: offset 0
;; 02       offset 1
;;          ...
;; 2(n-1)   offset n-1
;;          len0,string 0
;;          len1,string 1
;;          ...
macro mktab tab,[str]
{
    align 2
    common
      label tab word
    forward
      local s
      dw s-tab
    forward
      local e
      s db e-$-1,str
      label e
}

;; conditional jump mnemonics
mktab    jcctab,\
         'JO',\
         'JNO',\
         'JB',\
         'JNB',\
         'JZ',\
         'JNZ',\
         'JBE',\
         'JA',\
         'JS',\
         'JNS',\
         'JP',\
         'JNP',\
         'JL',\
         'JNL',\
         'JLE',\
         'JG'

;; core logic/arithmetic mnemonics
mktab    mntab,\
         'ADD',\
         'OR',\
         'ADC',\
         'SBB',\
         'AND',\
         'SUB',\
         'XOR',\
         'CMP',\
         'MOV',\
         'LOOPNZ',\
         'LOOPZ',\
         'LOOP',\
         'JCXZ'

;; register names
mktab    regtab,\
         'AL',\
         'CL',\
         'DL',\
         'BL',\
         'AH',\
         'CH',\
         'DH',\
         'BH',\
         'AX',\
         'CX',\
         'DX',\
         'BX',\
         'SP',\
         'BP',\
         'SI',\
         'DI'

;; effective address expressions
mktab    efftab,\
         'BX+SI',\
         'BX+DI',\
         'BP+SI',\
         'BP+DI',\
         'SI',\
         'DI',\
         'BP',\
         'BX'

mktab   errtab,\
        'unknown opcode',\
        'usage: 1_3 path',\
        'unexpected end of input'

hextab  db '0123456789abcdef'


; jump tables ------------------------------------------------------------------

align 8

;; brute force opcode byte dispatch, all 256 values
optab    dq mr8,mr16,rm8,rm16,ai8,ai16,opq,opq                  ; 00 - 07
         dq mr8,mr16,rm8,rm16,ai8,ai16,opq,two                  ; 08 - 0F
         dq mr8,mr16,rm8,rm16,ai8,ai16,opq,opq                  ; 10 - 17
         dq mr8,mr16,rm8,rm16,ai8,ai16,opq,opq                  ; 18 - 1F
         dq mr8,mr16,rm8,rm16,ai8,ai16,opq,opq                  ; 20 - 27
         dq mr8,mr16,rm8,rm16,ai8,ai16,opq,opq                  ; 28 - 2F
         dq mr8,mr16,rm8,rm16,ai8,ai16,opq,opq                  ; 30 - 37
         dq mr8,mr16,rm8,rm16,ai8,ai16,opq,opq                  ; 38 - 3F
         dq 8 dup opq                                           ; 40 - 47
         dq 8 dup opq                                           ; 48 - 4F
         dq 8 dup opq                                           ; 50 - 57
         dq 8 dup opq                                           ; 58 - 5F
         dq 8 dup opq                                           ; 60 - 67
         dq 8 dup opq                                           ; 68 - 6F
         dq 8 dup jcc                                           ; 70 - 77
         dq 8 dup jcc                                           ; 78 - 7F
         dq mi8,mi16,opq,m16i8,4 dup opq                        ; 80 - 87
         dq mvmr8,mvmr16,mvrm8,mvrm16,4 dup opq                 ; 88 - 8F
         dq 8 dup opq                                           ; 90 - 97
         dq 8 dup opq                                           ; 98 - 9F
         dq mvam8,mvam16,mvma8,mvma16,4 dup opq                 ; A0 - A7
         dq 8 dup opq                                           ; A8 - AF
         dq 8 dup mvri8                                         ; B0 - B7
         dq 8 dup mvri16                                        ; B8 - BF
         dq 6 dup opq,mvmi8,mvmi16                              ; C0 - C7
         dq 8 dup opq                                           ; C8 - CF
         dq 8 dup opq                                           ; D0 - D7
         dq 8 dup opq                                           ; D8 - DF
         dq 4 dup _loop,4 dup opq                               ; E0 - E7
         dq 8 dup opq                                           ; E8 - EF
         dq 8 dup opq                                           ; F0 - F7
         dq 8 dup opq                                           ; F8 - FF

;; ModR/M MOD field dispatch
modtab  dq mod00,mod01,mod10,mod11


;; CODE ========================================================================

;; I/O routines ----------------------------------------------------------------

getb:   add     rsi,1
        cmp     rsi,r15
        ja      erreof
        movzx   eax,byte [rsi-1]
        ret

getw:   add     rsi,2
        cmp     rsi,r15
        ja      erreof
        movzx   eax,word [rsi-2]
        ret

;; XXX clean up
putb:   push    rsi
        mov     ecx,eax
        shr     ecx,4
        lea     esi,[hextab+rcx]
        movsb
        mov     ecx,eax
        and     ecx,$f
        lea     esi,[hextab+rcx]
        movsb
        pop     rsi
        ret

;; XXX clean up
putw:   push    rax
        movzx   eax,ah
        test    eax,eax
        jz      @f
        call    putb
@@:     pop     rax
        push    rax
        movzx   eax,al
        call    putb
        pop     rax
        ret

;; type string from a string table to output
;; EAX = string number {0,1,..}
;; ECX = table address
type:   push    rsi
        movzx   esi,word [rcx+rax*2]            ; string offset
        add     esi,ecx                         ;   from base
        lodsb                                   ; string length
        movzx   ecx,al
        rep     movsb                           ; type string
        pop     rsi
        ret


;; common small string subroutines ---------------------------------------------

crlf:   mov     ax,$0a0d
        stosw
        ret

plus:   mov     al,'+'
        stosb
        ret

minus:  mov     al,'-'
        stosb
        ret

lbrak:  mov     al,'['
        stosb
        ret

rbrak:  mov     al,']'
        stosb
        ret

comma:  mov     al,','
        stosb
        ret

dollar: mov     al,'$'
        stosb
        ret

_byte:  mov     eax,'BYTE'
        stosd
space:  mov     al,' '
        stosb
        ret

_word:  mov     eax,'WORD'
        stosd
        jmp     space


;; common decoding subroutines -------------------------------------------------

;; type instruction mnemonic followed by space
;; eax = mnemonic index
mnem:   mov     ecx,mntab
        call    type
        jmp     space

;; read imm8 and output unsigned hex
imm8:   call    dollar
        call    getb
        jmp     putb

;; read imm16 and output unsigned hex
imm16:  call    dollar
        call    getw
        jmp     putw

;; type register component of effective address expression
aregs:  mov     eax,edx
        and     eax,7
        mov     ecx,efftab
        jmp     type

;; decode r/m given mod = 00b
;; edx = r/m field
mod00:  call    lbrak
        cmp     edx,6                   ; R/M = 110b?
        je      @f                      ;   absolute 16-bit offset
        call    aregs
        jmp     rbrak

@@:     call    imm16
        jmp     rbrak

;; decode r/m given mod = 01b
;; edx = r/m field
mod01:  call    lbrak
        call    aregs
        call    getb
        test    eax,eax
        jz      rbrak                   ; elide 0
        mov     cl,al
        test    al,al
        js      @f
        call    plus
        jmp     .disp
@@:     neg     cl
        call    minus
.disp:  call    dollar
        mov     al,cl
        call    putb
        jmp     rbrak

;; decode r/m given mod = 10b
;; edx = r/m field
mod10:  call    lbrak
        call    aregs
        call    getw
        test    eax,eax
        jz      rbrak                   ; elide 0
        mov     cx,ax
        test    ax,ax
        js      @f
        call    plus
        jmp     .disp
@@:     neg     ecx
        call    minus
.disp:  call    dollar
        mov     ax,cx
        call    putw
        jmp     rbrak

;; decode reg or r/m w/mod=11b
;; edx = reg or r/m field [0,7]
;; ebx = 0 -> 8bit, 8 -> 16-bit
mod11:  lea     eax,[rdx+rbx]
        mov     ecx,regtab
        jmp     type

;; decode ModR/M effective address
;; eax = ModR/M byte (destroyed)
modrm:  mov     edx,eax
        shr     eax,6                   ; MOD
        and     edx,7                   ; R/M
        jmp     qword [modtab+rax*8]    ; dispatch thru mod

reg:    mov     edx,eax
        shr     edx,3
        and     edx,7
        jmp     mod11

moffs:  call    lbrak
        test    ebx,ebx
        jz      @f
        call    imm16
        jmp     rbrak
@@:     call    imm8
        jmp     rbrak

rel8:   call    dollar
        call    getb                    ; rel8 byte
        add     al,2                    ; inst length (XXX overflow )
        mov     bl,al
        jns     @f
        call    minus
        neg     bl
        jmp     .off
@@:     call    plus
.off:   call    dollar
        mov     al,bl
        jmp     putb


;; opcode table entries --------------------------------------------------------

;; OP? - unsupported opcode
opq:    xor     eax,eax
        jmp     errstr

;; TWO - two byte opcode
two:    jmp     opq

;; OP m/r8,r8
mr8:    xor     ebx,ebx
        jmp     @f

;; OP m/r16,r16
mr16:   mov     ebx,8
@@:     shr     eax,3
mr:     call    mnem
        call    getb
        push    rax
        call    modrm                   ; decode destination
        call    comma
        pop     rax
        jmp     reg

;; MOV r/m8,r8
mvmr8:  xor     ebx,ebx
        jmp     @f

;; MOV r/m16,r16
mvmr16: mov     ebx,8
@@:     mov     eax,8
        jmp     mr

;; OP r8,r/m8
rm8:    xor     ebx,ebx
        jmp     @f

;; OP r16,r/m16
rm16:   mov     ebx,8
@@:     shr     eax,3
rm:     call    mnem
        call    getb
        push    rax
        call    reg
        call    comma
        pop     rax
        jmp     modrm                   ; decode source

;; MOV r8,r/m8
mvrm8:  xor     ebx,ebx
        jmp     @f

;; MOV r16,r/m16
mvrm16: mov     ebx,8
@@:     mov     eax,8
        jmp     rm

;; OP al,imm8
ai8:    xor     ebx,ebx
        jmp     @f

;; OP ax,imm16
ai16:   mov     ebx,8
@@:     shr     eax,3
        call    mnem
        mov     eax,ebx
        mov     ecx,regtab
        call    type
        call    comma
        test    ebx,ebx
        jz      imm8
        jmp     imm16

;; OP r/m8,imm8
mi8:    xor     ebx,ebx
        jmp     @f

;; OP r/m16,imm16
mi16:   mov     ebx,8
@@:     call    getb
        shr     eax,3                   ; reg field
        and     eax,7                   ;   is the
        call    mnem                    ;   opcode extension
        movzx   eax,byte [rsi-1]
        mov     ecx,eax
        shr     ecx,6
        cmp     ecx,3                   ; mod = 11? register
        je      @f
        mov     ecx,_byte
        mov     edx,_word
        test    ebx,ebx
        cmovz   edx,ecx
        call    rdx
        movzx   eax,byte [rsi-1]
@@:     call    modrm
        call    comma
        test    ebx,ebx
        jz      imm8
        jmp     imm16

;; OP r/m16,imm8
m16i8:  mov     ebx,8
        call    getb
        shr     eax,3                   ; reg field
        and     eax,7                   ;   is the
        call    mnem                    ;   opcode extension
        movzx   eax,byte [rsi-1]
        mov     ecx,eax
        shr     ecx,6
        cmp     ecx,3
        jz      @f
        call    _word
        movzx   eax,byte [rsi-1]
@@:     call    modrm
        call    comma
        jmp     imm8

;; Jcc rel8
jcc:    and     eax,15
        mov     ecx,jcctab
        call    type
        call    space
        jmp     rel8


;; MOV r8,i8
mvri8:  xor     ebx,ebx
        jmp     @f

;; MOV r16,i16
mvri16: mov     ebx,8
@@:     mov     eax,8
        call    mnem
        movzx   edx,byte [rsi-1]
        and     edx,7
        call    mod11
        call    comma
        test    ebx,ebx
        jz      imm8
        jmp     imm16

;; MOV r/m8,imm8
mvmi8:  xor     ebx,ebx
        jmp     @f

;; MOV r/m16,imm16
mvmi16: mov     ebx,8
@@:     mov     eax,8
        call    mnem
        call    getb
        call    modrm
        call    comma
        test    ebx,ebx
        jz      @f
        call    _word
        jmp     imm16
@@:     call    _byte
        jmp     imm8

;; MOV al,moffs8
mvam8:  xor     ebx,ebx
        jmp     @f

;; MOV ax,moffs16
mvam16: mov     ebx,8
@@:     mov     eax,8
        call    mnem
        mov     eax,ebx
        mov     ecx,regtab
        call    type
        call    comma
        jmp     moffs

;; MOV moffs16,ax
mvma8:  xor     ebx,ebx
        jmp     @f

;; MOV moffs8,ax
mvma16: mov     ebx,8
@@:     mov     eax,8
        call    mnem
        call    moffs
        call    comma
        mov     eax,ebx
        mov     ecx,regtab
        jmp     type

;; LOOP variants, including JCXZ
_loop:  and     eax,3
        add     eax,9
        call    mnem
        jmp     rel8


;; main decode loop ------------------------------------------------------------

;; next instruction, leading with new line for all but first
next:   call    crlf

go:     call    getb                    ; read opcode byte
        call    qword [optab+rax*8]     ; dispatch on byte

        ;; emit instruction
        push    rsi
        mov     esi,instbuf             ; source
        mov     ecx,edi
        sub     ecx,esi                 ; # bytes
        mov     edi,[outp]
        lea     edx,[rdi+rcx]
        cmp     edx,OUTLEN              ; new outp <= OUTLEN?
        jbe     @f                      ;   yes, skip flushing

        ;; flush output buffer
        push    rcx                     ; save instruction length
        call    flush                   ; flush output
        pop     rcx                     ; restore instruction length
        mov     edx,ecx
        xor     edi,edi
        mov     esi,instbuf

@@:     ;; copy instruction to output buffer
        mov     [outp],edx              ; update outp
        add     edi,outbuf              ; dest address in output buffer
        rep     movsb                   ; copy bytes
        pop     rsi                     ; restore input pointer
        mov     edi,instbuf             ; reset instruction pointer
        cmp     rsi,r15                 ; end of input?
        jb      next                    ;   no, decode next instruction

        ;; flush pending output and exit
        call    flush
        jmp     exit                    ; exit

flush:  ;; write outbuf to stdout and reset outp
        invoke  WriteFile,r14d,outbuf,[outp],temp,0
        test    eax,eax
        jz      errsys
        xor     eax,eax
        mov     [outp],eax
        ret


;; WINDOWS ENTRY ---------------------------------------------------------------
align 16
start:
        sub     rsp,8                   ; 16 byte ABI alignment

        ;; get stderr handle
        invoke  GetStdHandle,dword -12
        cmp     eax,-1
        je      exit
        mov     r13d,eax

        ;; get stdout handle
        invoke  GetStdHandle,dword -11
        cmp     eax,-1
        je      errsys
        mov     r14d,eax

        ;; parse command line, expect 2 args: image + input file path
        invoke  GetCommandLine
        invoke  CommandLineToArgv,rax,temp
        cmp     dword [temp],2
        jne     erruse

        ;; open input file
        mov     rax,[rax+8]
        invoke  CreateFile,rax,GENERIC_READ,1,0,3,$80,0
        cmp     eax,-1
        je      errsys
        mov     ebx,eax

        ;; get file size
        invoke  GetFileSizeEx,ebx,temp
        test    eax,eax
        jz      errsys
        mov     r15,qword [temp]
        test    r15,r15                 ; empty file?
        jz      exit                    ;   yes, already done

        ;; create a mapping object
        invoke  CreateFileMapping,ebx,0,PAGE_READONLY,0,0,0
        test    rax,rax
        jz      errsys

        ;; map entire input file into address space
        invoke  MapViewOfFile,rax,FILE_MAP_READ,0,0,0
        test    rax,rax
        jz      errsys
        mov     rsi,rax                 ; mapped input base address
        add     r15,rax                 ; base + length = end address

        ;; finally, decode input
        xor     edi,edi
        mov     [outp],edi              ; init outp
        mov     edi,instbuf             ; init inst buffer
        jmp     go


;; EXITS -----------------------------------------------------------------------

;; XXX janky "and rsp,-16" to fix stack for ABI
;;     ** only works because we don't return once in here **

errsys: and     rsp,-16
        invoke  GetLastError
        invoke  FormatMessage,$1100,0,eax,0,temp,0,0
        mov     ebx,eax
        mov     rax,qword [temp]
errmsg: and     rsp,-16
        invoke  WriteFile,r13d,rax,ebx,temp,0
        mov     eax,1
exit:   and     rsp,-16
        invoke  ExitProcess,eax

errstr: movzx   eax,word [errtab+rax*2]
        add     eax,errtab+1
        movzx   ebx,byte [rax-1]
        jmp     errmsg

erruse: mov     eax,1
        jmp     errstr

erreof: mov     eax,2
        jmp     errstr


section '.data' readable writeable

temp     dd ?
outp     dd ?                           ; output position
instbuf  db 64-($-$$) dup ?             ; instruction buffer
outbuf   db 4096-($-$$) dup ?           ; output buffer
OUTLEN   = $ - outbuf

section '.idata' import data readable writeable

  library kernel32,'KERNEL32.DLL',\
          shell32,'SHELL32.DLL'

  import  kernel32,\
          CreateFile,'CreateFileW',\
          CreateFileMapping,'CreateFileMappingA',\
          ExitProcess,'ExitProcess',\
          FormatMessage,'FormatMessageA',\
          GetCommandLine,'GetCommandLineW',\
          GetFileSizeEx,'GetFileSizeEx',\
          GetLastError,'GetLastError',\
          GetStdHandle,'GetStdHandle',\
          MapViewOfFile,'MapViewOfFile',\
          WriteFile,'WriteFile'

  import  shell32,\
          CommandLineToArgv,'CommandLineToArgvW'
