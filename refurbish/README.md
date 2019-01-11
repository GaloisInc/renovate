# Refurbish

## Tools

### QEMU Docker Runner `refurbish-run`

Run an executable via QEMU in a Docker container.

Usage

    refurbish-run EXE ARGS

where `EXE` is a local executable on the host machine, and `ARGS` are
zero or more args to `EXE`. The `EXE` gets copied from the host
machine into the Docker container and run there.

For example

      stack exec -- refurbish-run /bin/ls -la /

lists the contents of the root directory in the Docker container.

