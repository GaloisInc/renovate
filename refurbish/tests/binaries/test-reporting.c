#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <signal.h>
#include <unistd.h>

// #define PRINTF(...) printf(__VA_ARGS__)
#define PRINTF(...) do {} while(0)

typedef int (*function_pointer)();

int do_bad_stuff(void) {
  char data[] = {0xC3, 0xC3, 0xC3, 0xC3};
  function_pointer fun_pointer = (function_pointer) data;
  return fun_pointer(); // indirect call
}

int main() {
  intptr_t reporting_addr = (1 << 11) * getpagesize(); // some high-numbered page
  PRINTF("reporting address: %p\n", (void*)reporting_addr);
  size_t *region =
    mmap((void *) reporting_addr,
         sizeof(size_t), // pointer size
         PROT_READ // can be read
         |PROT_WRITE, // can be written
         MAP_ANONYMOUS // not file-backed, zero-initialized
         |MAP_SHARED  // shared block of memory
         |MAP_FIXED, // force exact positioning
         0,
         0);
  if (region == MAP_FAILED) {
    return -1;
  }
  pid_t pid = fork();
  if (pid == -1) {
    // could not fork
    return -1;
  }
  if (pid == 0) {
    PRINTF("In child process\n");
    return do_bad_stuff();
  }
  PRINTF("In parent process\n");
  int child_status = 0;
  waitpid(pid, &child_status, 0);
  int signaled = WIFSIGNALED(child_status);
  PRINTF("Child exited with signal? %d\n", signaled);
  if (signaled) {
    int signal = WTERMSIG(child_status);
    switch (signal) {
      case SIGSEGV: {
        PRINTF("Child exited with SIGSEGV (before embrittling)\n");
        return 0;
      }
      case SIGTRAP: {
        PRINTF("Child exited with SIGTRAP (because of embrittling)\n");
        if ((*region) == 0) {
          PRINTF("No reporting found\n");
          return 1;
        }
        PRINTF("Found reporting\n");
        return 2;
        break;
      }
      default: {
        PRINTF("Child exited with unknown signal! %d\n", signal);
        return 3;
      }
    }
  }
  return 4;
}
