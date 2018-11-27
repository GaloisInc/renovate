int g = 1;
void target() {
  g = 1;
}

void _start() {
  target();
  asm volatile                                  \
     ("li 0,1\n"                                \
      "li 3,1\n"                                \
         "sc");
}
