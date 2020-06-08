# To build via nix: $ nix-shell -p pkgs.gcc --run 'make -f Makefile.x86_64'

x86exe = $(patsubst %.c,%.nostdlib.x86_64.exe,$(wildcard *.c)) $(patsubst %.c,%.musl.x86_64.exe,$(wildcard *.c)) $(patsubst %.c,%.glibc.x86_64.exe,$(wildcard *.c))
# ppc32exe = $(patsubst %.c,%.nostdlib.ppc32.exe,$(wildcard *.c)) $(patsubst %.c,%.stdlib.ppc32.exe,$(wildcard *.c))
ppc64exe = $(patsubst %.c,%.nostdlib.ppc64.exe,$(wildcard *.c)) $(patsubst %.c,%.stdlib.ppc64.exe,$(wildcard *.c))
aarch32exe = $(patsubst %.c,%.noopt.nostdlib.aarch32.exe,$(wildcard *.c)) $(patsubst %.c,%.noopt.stdlib.aarch32.exe,$(wildcard *.c)) $(patsubst %.c,%.opt.nostdlib.aarch32.exe,$(wildcard *.c))  $(patsubst %.c,%.opt.stdlib.aarch32.exe,$(wildcard *.c))

all: $(x86exe) $(ppc64exe) $(aarch32exe)

x86muslcc = musl-gcc
x86cc = gcc
ppc32cc = powerpc-linux-gnu-gcc
ppc64cc = powerpc64-linux-gnu-gcc
aarch32cc = arm-linux-gnueabi-gcc

CFLAGS = -static -fno-stack-protector
aarch32_cflags = -fno-pie -fno-pic

%.nostdlib.x86_64.exe: %.c
	$(x86cc) -DNOSTDLIB -nostdlib $(CFLAGS) $< -o $@

%.musl.x86_64.exe: %.c
	$(x86muslcc) $(CFLAGS) -mno-avx -mno-avx2 $< -o $@

%.glibc.x86_64.exe: %.c
	$(x86cc) $(CFLAGS) -mno-avx -mno-avx2 $< -o $@

%.ppc32.exe: %.c
	$(ppc32cc) $(CFLAGS) $< -o $@

%.nostdlib.ppc64.exe: %.c
	$(ppc64cc) -DNOSTDLIB -nostdlib $(CFLAGS) $< -o $@

%.stdlib.ppc64.exe: %.c
	$(ppc64cc) $(CFLAGS) $< -o $@

%.noopt.nostdlib.aarch32.exe: %.c util.h
	$(aarch32cc) $(aarch32_cflags) -DNOSTDLIB -nostdlib $(CFLAGS) $< -o $@

%.noopt.stdlib.aarch32.exe: %.c util.h
	$(aarch32cc) $(aarch32_cflags) $(CFLAGS) $< -o $@

clean:
	rm -f *.exe

