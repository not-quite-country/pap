;; PAP 1.2 - decode all the non-segment MOVs

format pe64 console
entry start

include 'win64w.inc'

section '.text' code readable executable

optab           dq movModRm,err_opcode,movAcc,movRegImm,movMemImm
hextab          db '0123456789abcdef'
regs            db 'AL','CL','DL','BL','AH','CH','DH','BH'
                db 'AX','CX','DX','BX','SP','BP','SI','DI'
addr2tab        db 'SI','DI','BP','BX'
addr5tab        db 'BX+SI','BX+DI','BP+SI','BP+DI'

usage   db 'Usage: 1_2 input_file',10,13
usage_len = $-usage

badop   db 'Unsupported opcode',10,13
badop_len = $-badop

badmod  db 'Unsupported Mod field in ModR/M byte',10,13
badmod_len = $-badmod

badeof  db 'Unexpected end of input',10,13
badeof_len = $-badeof

;; emit 'MOV '
_mov:
        mov     eax,'MOV '
        stosd
        ret

;; emit windows newline
crlf:
        mov     ax,$0a0d
        stosw
        ret

;; emit '+'
plus:
        mov     al,'+'
        stosb
        ret

;; emit '-'
minus:
        mov     al,'-'
        stosb
        ret

;; emit '['
lbrak:
        mov     al,'['
        stosb
        ret

;; emit ']'
rbrak:
        mov     al,']'
        stosb
        ret

;; emit ','
comma:
        mov     al,','
        stosb
        ret

;; emit ' '
space:
        mov     al,' '
        stosb
        ret

;; emit '$'
dollar:
        mov     al,'$'
        stosb
        ret

;; emit 'byte '
_byte:
        mov     eax,'byte'
        stosd
        jmp     space

;; emit 'word '
_word:
        mov     eax,'word'
        stosd
        jmp     space

;; get next byte from input or abort if eof
;; eax := zero extended byte
getb:
        add     rsi,1
        cmp     rsi,r15
        ja      err_eof
        movzx   eax,byte [rsi-1]
        ret

;; get next word (2 bytes) from input or abort if eof
;; eax := zero extended word
getw:
        add     rsi,2
        cmp     rsi,r15
        ja      err_eof
        movzx   eax,word [rsi-2]
        ret

;; put byte as hex into instruction buffer
putb:
        push    rsi
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

;; put word as hex into instruction buffer
putw:
        push    rax
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

;; translate imm8
imm8:
        call    dollar
        call    getb
        jmp     putb

;; translate imm16
imm16:
        call    dollar
        call    getw
        jmp     putw

;; emit address computation expression
aregs:
        push    rsi
        mov     eax,edx
        and     eax,3
        test    edx,4
        jz      @f
        lea     esi,[addr2tab+rax+rax]
        movsw
        pop     rsi
        ret
@@:     lea     esi,[addr5tab+rax+rax*4]
        movsd
        movsb
        pop     rsi
        ret

;; decode r/m given mod = 00b
;; edx = r/m field
;; ebp = operand width
mod00:
        call    lbrak
        cmp     edx,6                   ; R/M = 110b?
        je      @f                      ;   absolute 16-bit offset
        call    aregs
        jmp     rbrak
@@:     call    imm16
        jmp     rbrak

;; decode r/m given mod = 01b
;; edx = r/m field
;; ebp = operand width
mod01:
        call    lbrak
        call    aregs
        call    getb
        test    eax,eax
        jz      rbrak                   ; don't need + 0
        movsx   eax,al                  ; need sign for disp8
        test    eax,eax
        js      @f
        push    rax
        call    plus
        jmp     .disp
@@:     neg     eax
        push    rax
        call    minus
.disp:  call    dollar
        pop     rax
        call    putb
        jmp     rbrak

;; decode r/m given mod = 10b
;; edx = r/m field
;; ebp = operand width
mod10:
        call    lbrak
        call    aregs
        call    getw
        test    eax,eax
        jz      rbrak                   ; don't need + 0
        movsx   eax,ax                  ; need sign for disp16
        test    eax,eax
        js      @f
        push    rax
        call    plus
        jmp     .disp
@@:     neg     eax
        push    rax
        call    minus
.disp:  call    dollar
        pop     rax
        call    putw
        jmp     rbrak

;; decode r/m given mod = 11b
;; edx = r/m field
;; ebp = operand width
mod11:
        push    rsi
        lea     esi,[regs+rbp+rdx*2]
        movsw
        pop     rsi
        ret

; decode ModR/M effective address
; eax = ModR/M byte (destroyed)
; ebp = operand width
modrm:
        mov     edx,eax
        shr     eax,6                   ; MOD
        and     edx,7                   ; R/M
        lea     rax,[@f+rax+rax*4]      ; index into jump table
        jmp     rax                     ; dispatch
@@:     jmp     near mod00
        jmp     near mod01
        jmp     near mod10
        jmp     near mod11

;; general ModR/M mov
movModRm:
        mov     ebp,edx
        and     ebp,1
        shl     ebp,4                   ; 0 = 8-bit, 16 = 16-bit
        shr     edx,1
        sub     edx,4
        jb      err_opcode
        cmp     edx,1
        ja      err_opcode
        call    getb
        push    rax                     ; save a copy of ModR/M byte
        test    edx,1
        jnz     @f                      ; mov r,r/m

        ;; mov r/m,r
        call    modrm                   ; decode destination
        call    comma
        pop     rdx                     ; recover ModR/M byte
        shr     edx,3
        and     edx,7                   ; REG field
        jmp     mod11                   ; decode register source

@@:     ;; mov r,r/m
        mov     edx,eax
        shr     edx,3
        and     edx,7                   ; REG field
        call    mod11                   ; decode register destination
        call    comma
        pop     rax                     ; recover ModR/M byte
        jmp     modrm                   ; decode source

;; emit accumulator depending on operand width
;; ebp = operand width
accum:
        push    rsi
        lea     esi,[regs+rbp]
        movsw
        pop     rsi
        ret

;; decode moff depending on operand width
;; ebp = operand width
moff:
        call    lbrak
        test    ebp,ebp
        jnz     @f
        call    imm8
        jmp     rbrak
@@:     call    imm16
        jmp     rbrak

;; accumulator mov
movAcc:
        mov     ebp,edx
        and     ebp,1
        shl     ebp,4                   ; 0 = 8-bit, 16 = 16-bit
        test    edx,$c                  ; expect low 2 bits of opcode = 00b
        jnz     err_opcode              ;  no, unknown opcode
        test    edx,2                   ; store accumulator?
        jnz     @f                      ;   yes

        ;; load accumulator
        call    accum
        call    comma
        jmp     moff

@@:     ;; store accumulator
        call    moff
        call    comma
        jmp     accum

;; immediate-to-reg mov
movRegImm:
        mov     ebp,edx
        shr     ebp,3
        shl     ebp,4                   ; 0 = 8-bit, 16 = 16-bit
        and     edx,7                   ; REG
        call    mod11                   ; decode destination register
imm:
        call    comma
        test    ebp,ebp
        jnz     imm16
        jmp     imm8

;; immediate-to-mem mov
movMemImm:
        mov     ebp,edx
        and     ebp,1
        shl     ebp,4                   ; 0 = 8-bit, 16 = 16-bit
        shr     edx,1
        cmp     edx,3
        jne     err_opcode
        call    getb                    ; get ModR/M byte
        test    eax,$38                 ; expect REG = 000b
        jnz     err_opcode              ;   no, encoding error (XXX doesn't matter)
        mov     edx,eax
        call    modrm                   ; decode destination
        call    comma
        test    ebp,ebp
        jnz     @f                      ; word
        call    _byte
        jmp     imm8
@@:     call    _word
        jmp     imm16


;; next instruction, leading with new line for all but first
next:
        call    crlf
decode:
        ;; all ops are assumed to be MOV
        call    _mov

        ;; opcode dispatch on 4-bit prefix
        call    getb                    ; read opcode byte
        mov     ecx,eax
        mov     edx,eax
        and     edx,$f                  ; low nibble
        shr     ecx,4                   ; high nibble
        sub     ecx,8                   ; rescale to 0
        jb      err_opcode              ; prefix < 8 => bad opcode
        cmp     ecx,4
        ja      err_opcode              ; prefix > $c => bad opcode
        call    qword [optab+rcx*8]     ; decode instruction

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
        jz      err_last
        xor     eax,eax
        mov     [outp],eax
        ret

;; --- WINDOWS ENTRY ---
align 16
start:
        sub     rsp,8                   ; 16 byte ABI alignment

        ;; get stderr handle
        invoke  GetStdHandle,dword -12
        cmp     eax,-1
        je      err_exit
        mov     r13d,eax

        ;; get stdout handle
        invoke  GetStdHandle,dword -11
        cmp     eax,-1
        je      err_last
        mov     r14d,eax

        ;; parse command line, expect 2 args: image + input file path
        invoke  GetCommandLine
        invoke  CommandLineToArgv,rax,temp
        cmp     dword [temp],2
        jne     err_usage

        ;; open input file
        mov     rax,[rax+8]
        invoke  CreateFile,rax,GENERIC_READ,1,0,3,$80,0
        cmp     eax,-1
        je      err_last
        mov     ebx,eax

        ;; get file size
        invoke  GetFileSizeEx,ebx,temp
        test    eax,eax
        jz      err_last
        mov     r15,qword [temp]
        test    r15,r15                 ; empty file?
        jz      exit                    ;   yes, already done

        ;; create a mapping object
        invoke  CreateFileMapping,ebx,0,PAGE_READONLY,0,0,0
        test    rax,rax
        jz      err_last

        ;; map entire input file into address space
        invoke  MapViewOfFile,rax,FILE_MAP_READ,0,0,0
        test    rax,rax
        jz      err_last
        mov     rsi,rax                 ; mapped input base address
        add     r15,rax                 ; base + length = end address

        ;; finally, decode input
        xor     edi,edi
        mov     [outp],edi              ; init outp
        mov     edi,instbuf             ; init inst buffer
        jmp     decode

;; --- EXITS ---
err_last:
        invoke  GetLastError
        invoke  FormatMessage,$1100,0,eax,0,temp,0,0
        mov     ebx,eax
        mov     rax,qword [temp]
err_msg:
        invoke  WriteFile,r13d,rax,ebx,temp,0
err_exit:
        mov     eax,1
exit:
        invoke  ExitProcess,eax

err_usage:
        mov     eax,usage
        mov     ebx,usage_len
        jmp     err_msg

err_opcode:
        mov     eax,badop
        mov     ebx,badop_len
        jmp     err_msg

err_mod:
        mov     eax,badmod
        mov     ebx,badmod_len
        jmp     err_msg

err_eof:
        mov     eax,badeof
        mov     ebx,badeof_len
        jmp     err_msg

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
