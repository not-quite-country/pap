;; PAP 1.1 - disassemble real mode reg-reg MOV instructions

format pe64 console
entry start

include 'win64w.inc'            ; FASM supplied Windows API stuff

section '.text' code readable executable

  CRLF    = $0d0a
  COMMA   = $2c

  regs    db 'AL','CL','DL','BL','AH','CH','DH','BH'
          db 'AX','CX','DX','BX','SP','BP','SI','DI'

  regoff  db 0,16,0,16

  movstr  db 'MOV '

  usage   db 'Usage: 1_1 input_file',10,13
  usage_len = $-usage

  badop   db 'Unsupported opcode',10,13
  badop_len = $-badop

  badmod  db 'Unsupported Mod field in ModR/M byte',10,13
  badmod_len = $-badmod

  badeof  db 'Unexpected end of input',10,13
  badeof_len = $-badeof

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
    mov     r15,[temp]
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


    ;; --- BEGIN ACTUAL TASK AT HAND ---
    mov     edi,outbuf
  next:
    ;; fetch and rescale opcode to [0,3]
    lodsb
    sub     al,$88
    jb      err_opcode
    cmp     al,3
    ja      err_opcode
    movzx   ebx,al                  ; EBX := opcode

    ;; fetch and decode ModR/M byte
    cmp     rsi,r15                  ; end of file?
    jae     err_eof                 ;   yes, unexpected
    lodsb
    movzx   ecx,al
    movzx   edx,al
    shr     al,6                    ; Mod
    and     ecx,7                   ; R/M
    shr     edx,3
    and     edx,7                   ; REG
    cmp     al,3                    ; Mod = 11b?
    jne     err_mod                 ;  no, unsupported ModR/M

    ;; select reg table, dest, and src
    movzx   ebp,byte [regoff+rbx]
    test    ebx,2
    mov     eax,ecx
    cmovz   ecx,edx
    cmovz   edx,eax

    ;; output disassembled instruction
    lea     eax,[rdi+11]
    cmp     eax,ENDBUF
    jbe     @f
    call    flush
@@: mov     rbx,rsi
    mov     eax,'MOV '
    stosd
    lea     esi,[regs+rbp+rdx*2]
    movsw                           ; destination register
    mov     eax,COMMA
    stosb                           ; ','
    lea     esi,[regs+rbp+rcx*2]
    movsw                           ; source register
    mov     eax,CRLF
    stosw                           ; new line
    mov     rsi,rbx

    ;; repeat until end of input
    cmp     rsi,r15                 ; end of file?
    jb      next                    ;   no, continue

    ;; done. flush buffered output and exit
    call    flush
    xor     eax,eax
    jmp     exit

    ;; --- END ACTUAL TASK AT HAND ---

    ;; flush output buffer and reset
  flush:
    push    rsi
    sub     edi,outbuf              ; # of bytes to write
    invoke  WriteFile,r14d,outbuf,edi,temp,0
    pop     rsi
    test    eax,eax
    jz      err_last
    mov     edi,outbuf              ; reset output buffer
    ret

  err_last:
    invoke  GetLastError
    invoke  FormatMessage,$1100,eax,0,temp,0,0
    lea     ebx,[rax*2]
    mov     rax,[temp]
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

section '.data' data readable writeable

  temp      dq ?
  outbuf    db 4096 - ($-$$) dup ?
  ENDBUF    = $


section '.idata' import data readable writeable

  library kernel32,'KERNEL32.DLL',\
          shell32,'SHELL32.DLL'

  import  kernel32,\
          CreateFile,'CreateFileW',\
          CreateFileMapping,'CreateFileMappingA',\
          ExitProcess,'ExitProcess',\
          FormatMessage,'FormatMessageW',\
          GetCommandLine,'GetCommandLineW',\
          GetFileSizeEx,'GetFileSizeEx',\
          GetLastError,'GetLastError',\
          GetStdHandle,'GetStdHandle',\
          MapViewOfFile,'MapViewOfFile',\
          WriteFile,'WriteFile'

  import  shell32,\
          CommandLineToArgv,'CommandLineToArgvW'
