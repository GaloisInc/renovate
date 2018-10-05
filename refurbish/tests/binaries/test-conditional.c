#include "util.h"

int g = -11;

void entry() {
  if(g > 0) {
    g = g + 1;
  }
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
