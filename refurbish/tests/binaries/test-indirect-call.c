#include "util.h"

int g = -11;

int (*fptr)(int);

int callee(int x) {
  return x * 2;
}

void entry() {
  fptr = callee;
  g = fptr(g);
}

#if defined(NOSTDLIB)
void _start() {
  entry();
  EXIT(0);
}
#else
int main() {
  entry();
  return 0;
}
#endif
