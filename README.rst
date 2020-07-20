Overview
========

This library implements a static binary rewriter.

Static binary rewriting is a capability that allows a user to add (or remove) code to a binary without executing it (i.e., as opposed to a dynamic binary rewriter like Intel's PIN tool or valgrind).  The API presented operates at the level of *basic blocks*, which are sequences of straightline code with no branches into the middle.

The core of the binary rewriter is architecture-independent; each supported architecture requires a small amount of architecture-specific code.  The renovate library exposes two primary APIs right now: ``analyzeElf`` and ``rewriteElf`` to perform architecture-independent program analysis and rewriting, respectively.  In rewriting mode, the results of a pre-rewriting analysis pass are fed to the rewriter, allowing rewriting to be driven by sophisticated analysis.  There are some usage examples in the ``Refurbish.Tutorial`` module, as well as the command line ``refurbish`` tool itself.

Currently, statically-linked ELF binaries for the x86_64 and PowerPC architectures are supported.

Potential uses of this library include, but are not limited to:
* Architecture-independent binary analysis (including symbolic simulation)
* Arbitrary binary rewriting of whole programs
* Binary diversity (through randomized code re-layout)

Repository Layout
-----------------

The library is divided into three components:

* ``renovate`` implements the core binary rewriting functionality.
* ``renovate-ppc`` implements the PowerPC-specific backend.
* ``renovate-x86`` implements the x86_64-specific backend.

There is also another package named ``refurbish``, which provides some useful utilities built on top of ``renovate``, including a command line executable to demonstrate some simple binary rewriting patterns.

Building
--------

The dependencies of this project that are not on Hackage are specified using git submodules.  To build the code with a modern version of ``cabal``::

  $ git submodule update --init
  $ ln -s cabal.project.dist cabal.project
  $ cabal new-configure
  $ cabal new-build renovate-ppc renovate-x86

Using the Library
-----------------

This code is intended to be imported qualified::

  import qualified Renovate as R

The architecture-specific backends each expose *configurations*  that can be passed to ``analyzeElf`` and ``rewriteElf``.  There is a tutorial in the module ``Refurbish.Tutorial``.

Status
======

This codebase is a work in progress.  PowerPC (both 32 bit and 64 bit) and x86_64 support is reasonably robust.  Support for ARM (AArch32) is experimental.

Limitations
-----------

This library cannot guarantee that it can find all of the code in a program.  It will currently not allow basic blocks to be rewritten if they are part of a function that has not been completely discovered.  Common causes of missed code include unresolved indirect jumps within functions.

This library produces binaries that trigger buggy code paths in older versions of QEMU user-mode emulation. The test suite uses QEMU 4.2, so anything more recent than that is probably a good choice.

License
=======

This code is available under the BSD3 license and without any support.
