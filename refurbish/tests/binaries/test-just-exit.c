#include "util.h"

void entry() {
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
