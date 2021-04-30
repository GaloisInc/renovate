# Refurbish

## Tools

### QEMU Docker Runner `refurbish-run`

Run an executable via QEMU in a Docker container. Useful for running
cross compiled executables for foreign architectures on your local
machine.

Note that QEMU also makes it easy to run foreign arch binaries
directly on your local machine -- on Ubuntu 18.04 installing the
`qemu` package (which brings in a bunch of deps) seems to be enough to
let me e.g. run *statically linked* PPC ELF binaries on an x86_64
machine directly, as if they were a native x86_64 ELF
binaries. However, `refurbish-run` is still useful because it runs
your binaries in a sandbox, which is a nice bit of firewalling when
running possibly corrupt binaries (e.g the output of `refurbish` or
`embrittle`).

More details about QEMU magic here (`binfmt_misc` kernel module + user
mode QEMU), including how to deal with dynamically linked foreign arch
binaries (in the simplest case you can use
e.g. `(QEMU_LD_PREFIX=/usr/powerpc64-linux-gnu/ tmp/hello-world)` when
`tmp/hello_world` is a dynamically linked PPC ELF binary that uses
glibc):
https://ownyourbits.com/2018/06/13/transparently-running-binaries-from-any-architecture-in-linux-with-qemu-and-binfmt_misc/

#### Setting up Docker and QEMU

You need to install the Docker tool and user space QEMU. On Ubuntu
this is provided by the `docker.io` package (and not the `docker`
package!) and `qemu-user`:

    sudo apt install docker.io qemu-user

To run `docker` you need to be part of the `docker` group:

     sudo adduser `whoami` docker

However, adding yourself to groups doesn't take effect until you log
in again. You can re login in an existing shell with `su`:

    exec su `whoami`

You can now run `docker` and hence `refurbish-run` in that
shell. However, note that the first time you use `docker` via
`refurbish-run` it will probably appear to hang while it downloads a
bunch of stuff in the background.

If you want the group update to take effect everywhere you'll probably
need to log out completely. You can check which groups you belong to
in a given shell with the `id` command.

GOTCHA: if `docker` is available but user space QEMU (via `qemu-user`
package) is not, then you'll get weird failures like the following
from `refurbish-run <prog>` when `<prog>` is for a different arch than
the host system. Examples from trying to run PPC binaries on an x86_64
system:

    $ cabal run refurbish-run -- tests/binaries/float_min.noopt.nostdlib.ppc64.exe
    refurbish-run: fd:15: hGetContents: invalid argument (invalid byte sequence)

    $ cabal run refurbish-run -- tests/binaries/float_min.opt.nostdlib.ppc64.exe
    /tmp/float_min.opt.nostdlib.ppc64.exe: 1: /tmp/float_min.opt.nostdlib.ppc64.exe: Syntax error: "(" unexpected

After installing `qemu-user` everything works.

#### Usage

Usage

    refurbish-run EXE ARGS

where `EXE` is a local executable on the host machine, and `ARGS` are
zero or more args to `EXE`. The `EXE` gets copied from the host
machine into the Docker container and run there. The `stdout` and
`stderr` get copied back out of the container into the local `stdout`
and `stderr`, respectively, and the return code is returned locally.

As a contrived example,

    cabal run -- refurbish-run /bin/ls -la /

lists the contents of the root directory in the Docker container.

A more realistic example is to use `refurbish-run` to run a PPC binary
on an x86_64 machine. For example, to run a PPC binary from the SFE
project:

    cabal run -- refurbish-run tests/binaries/float_min.opt.nostdlib.ppc64.exe
