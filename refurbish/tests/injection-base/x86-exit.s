.text
.globl _exit

_exit:
  movq $0,%rdi
  movq $60,%rax
  syscall


