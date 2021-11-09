
; Registers
; General purpose
;    r*x is 64 bit, e*x is 32 bit, *x is 16 bit, *h is high 8 bit, *l is low 8 bit
;   rax: | eax | ax | ah | al |
;   rbx: | ebx | bx | bh | bl |
;   rcx: | ecx | cx | ch | cl |
;   rdx: | edx | dx | dh | dl |
;   rsi: | esi |
;   rdi: | edi |
; 
;   rsp: | esp | sp |; stack pointer
;   rbp: | ebp | bp |; base pointer
;        sp & bp are 16 bit
;
;   rip: | eip | ip |; instruction pointer
;
;   [rbp, rsp, rbx, rdi, rsi] are callee saved
;   All others are caller saved
;   Return value is in rax, or smaller registers (i.e. eax, ax, al)

section .text

global add_fn_Zu64_Zu64
add_fn_Zu64_Zu64:
   push rbp         ; Save base pointer
   mov rbp, rsp     ; Set new base pointer
   sub rsp, 0       ; Allocate space for local vars
   push rbx
   push rdi
   push rsi         ; Save callee saved registers

   ; Arguments are [rbp + offset + size]
   mov rax, [rbp + 0 + 8] ; First argument
   add rax, [rbp + 8 + 8] ; Second argument & add

   pop rsi          ; Restore callee saved registers
   pop rdi
   pop rbx
   mov rsp, rbp     ; Deallocate local vars
   pop rbp          ; Restore base pointer
   ret              ; actual return

; Note: this uses exclusively the r (64 bit) versions of registers
global example_call_fn
example_call_fn:
   push rbp         ; Save base pointer
   mov rbp, rsp     ; Set new base pointer
   sub rsp, 0       ; Allocate space for local vars
   push rbx
   push rdi
   push rsi         ; Save callee saved registers

   ; Arguments are [rbp + offset + size]

   pop rsi          ; Restore callee saved registers
   pop rdi
   pop rbx
   mov rsp, rbp     ; Deallocate local vars
   pop rbp          ; Restore base pointer
   ret              ; actual return

; Name mangling scheme (seperated by underscores)
; LABEL => NAME | TYPE | ARGS*
; TYPE => 'fn', 'const', 'method'
; ARGS => 'Z' | VARTYPE
; VARTYPE => u64, etc
