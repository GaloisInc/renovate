# To build via nix: $ nix-shell -p pkgs.gcc --run 'make -f Makefile.x86_64'

x86exe = $(patsubst %.c,%.nostdlib.x86_64.exe,$(wildcard *.c)) $(patsubst %.c,%.stdlib.x86_64.exe,$(wildcard *.c))
# ppc32exe = $(patsubst %.c,%.nostdlib.ppc32.exe,$(wildcard *.c)) $(patsubst %.c,%.stdlib.ppc32.exe,$(wildcard *.c))
ppc64exe = $(patsubst %.c,%.nostdlib.ppc64.exe,$(wildcard *.c)) $(patsubst %.c,%.stdlib.ppc64.exe,$(wildcard *.c))

all: $(x86exe) $(ppc64exe)

x86cc = gcc
ppc32cc = powerpc-linux-gnu-gcc
ppc64cc = powerpc64-linux-gnu-gcc

CFLAGS = -static -fno-stack-protector

%.nostdlib.x86_64.exe: %.c
	$(x86cc) -DNOSTDLIB -nostdlib $(CFLAGS) $< -o $@

%.stdlib.x86_64.exe: %.c
	$(x86cc) $(CFLAGS) $< -o $@

%.ppc32.exe: %.c
	$(ppc32cc) $(CFLAGS) $< -o $@

%.nostdlib.ppc64.exe: %.c
	$(ppc64cc) -DNOSTDLIB -nostdlib $(CFLAGS) $< -o $@

%.stdlib.ppc64.exe: %.c
	$(ppc64cc) $(CFLAGS) $< -o $@

clean:
	rm -f *.exe

