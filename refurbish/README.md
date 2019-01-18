# Refurbish

## Tools

### QEMU Docker Runner `refurbish-run`

Run an executable via QEMU in a Docker container. Useful for running
cross compiled executables for foreign architectures on your local
machine.

Usage

    refurbish-run EXE ARGS

where `EXE` is a local executable on the host machine, and `ARGS` are
zero or more args to `EXE`. The `EXE` gets copied from the host
machine into the Docker container and run there. The `stdout` and
`stderr` get copied back out of the container into the local `stdout`
and `stderr`, respectively, and the return code is returned locally.

As a contrived example,

    stack exec -- refurbish-run /bin/ls -la /

lists the contents of the root directory in the Docker container.

A more realistic example is to use `refurbish-run` to run a PPC binary
on an x86_64 machine. For example, to run a PPC binary from the SFE
project:

    stack exec -- refurbish-run tests/binaries/float_min.opt.nostdlib.ppc64.exe
