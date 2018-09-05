#if defined(__arm__) || defined(__thumb__)
/*TBD: rval value on exit */
#define EXIT(rval)                              \
    asm volatile ("svc #0")
#endif

#ifdef __powerpc__
/*TBD: rval value on exit */
#define EXIT(rval)                              \
  asm volatile                                  \
     ("li 0,1\n"                                \
      "sc")
#endif

#if defined(__x86_64) || defined(__i686__)
#define EXIT(rval)                              \
  asm volatile                                  \
     (" movq %0,%%rdi\n"                        \
      " movq $60,%%rax\n"                       \
      " syscall"                                \
      : /* no output */                         \
      : "g" (rval)                              \
      : /* no extra modifications */)
#endif
