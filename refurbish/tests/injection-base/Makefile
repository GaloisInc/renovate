all: x86-exit.bin ppc64-exit.bin injection-base.ppc64.exe

x86-exit.o: x86-exit.s
	as $< -o $@

ppc64-exit.o: ppc64-exit.s
	powerpc64-linux-gnu-as $< -o $@

x86-exit.bin: x86-exit.o
	objcopy --only-section=.text -O binary $< $@

ppc64-exit.bin: ppc64-exit.o
	objcopy --only-section=.text -O binary $< $@

injection-base.ppc64.exe: injection-base.c
	powerpc64-linux-gnu-gcc $< -o $@ -nostdlib

clean:
	rm *.o *.bin
