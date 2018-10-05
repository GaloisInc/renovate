#include <stdint.h>
#include "util.h"

double fmin(double a, double b)
{
  if (a <= b) {
    return a;
  } else {
    return b;
  }
}

#if defined(NOSTDLIB)
void _start()
{
  EXIT(0);
}
#else
int main() {
  return 0;
}
#endif
