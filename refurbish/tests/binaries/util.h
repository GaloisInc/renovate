#if defined(__arm__) || defined(__thumb__)
/*TBD: rval value on exit */
#define EXIT(rval)                              \
    asm volatile ("svc #0")
#endif

#ifdef __powerpc__
/* Use register variables so that GCC figures out which load
 * instruction to use (ld, li, or mr) */
#define EXIT(rval)                              \
  {                                             \
    register int r0 asm ("r0");                 \
    register int r3 asm ("r3");                 \
    r0 = 1;                                     \
    r3 = (rval);                                \
    asm volatile                                \
       ("sc"                                    \
        : /* no output */                       \
        : "r" (r0), "r" (r3)                    \
        : /* no extra modifications */);        \
  }
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
