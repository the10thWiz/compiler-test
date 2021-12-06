;fn main() {
;    let b = -1;
;    let a = b.abs() + 12;
;    printf(\"%n\", a);
;}

section .data

; Constants

EXIT_SUCCESS    equ     0       ; successful operation
SYS_exit        equ     60      ; call code for terminate

STDIN           equ     0       ; standard input
STDOUT          equ     1       ; standard output
STDERR          equ     2       ; standard error


printf_const_param db "%n", LF, NULL

section .text
   global main
main:
   push -1
   call b.abs ()
   add rax, 12
   push rax

   pop
   pop
   ret

   global _start
_start:
   call main
   mov rax, SYS_exit
   mov rdi, 1;EXIT_SUCCESS
   syscall

