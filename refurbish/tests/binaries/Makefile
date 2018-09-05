# To build via nix: $ nix-shell -p pkgs.gcc --run 'make -f Makefile.x86_64'

x86exe = $(patsubst %.c,%.x86_64.exe,$(wildcard *.c))
# ppc32exe = $(patsubst %.c,%.ppc32.exe,$(wildcard *.c))
ppc64exe = $(patsubst %.c,%.ppc64.exe,$(wildcard *.c))

all: $(x86exe) $(ppc64exe)

x86cc = gcc
ppc32cc = powerpc-linux-gnu-gcc
ppc64cc = powerpc64-linux-gnu-gcc

CFLAGS = -static -nostdlib -fno-stack-protector

%.x86_64.exe: %.c
	$(x86cc) $(CFLAGS) $< -o $@

%.ppc32.exe: %.c
	$(ppc32cc) $(CFLAGS) $< -o $@

%.ppc64.exe: %.c
	$(ppc64cc) $(CFLAGS) $< -o $@

clean:
	rm -f *.exe

