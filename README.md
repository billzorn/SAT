# SAT
A Solver Aided Toolchain for building accurate CPU emulators from scratch.

## Overview

The central premise of SAT is that if we have a microprocessor available, we can
use its observable behavior to create an emulator which is accurate by
construction, rather than rely on manuals and documentation (which may leave
behavior "undefined" or simply omit aspects of it).  This toolchain accomplishes
this goal for simple microprocessors such as the TI MSP430 (used in their
Launchpad platform) by using a debug interface to collect data combined with
software synthesis and an extensible emulator framework. 

## The Pipeline

There are three stages to SAT.

1. Measurement.

In the measurement stage, the microprocessor's debug interface is used to
program a harness that executes instructions with known inputs and then collects
the outputs. For operations with a small input space, such as 8 bit operations,
it's feasible to collect data for all possible combinations of inputs. For
operations with a larger input space, we use dense random sampling to get a
representative portion of inputs that is likely to be sufficient for synthesis.
This data is accumulated into a series of input/output tables, which are saved
to disk for later use. 

2. Synthesis

In the synthesis stage, once we have data about the microprocessor's behavior
over a sufficiently large number of inputs, we use the data to generate
assertions that can be fed into an SMT solver to find a program which yields
correct results for all known inputs.  In this pipeline, we use
[Synapse](https://github.com/uwplse/synapse), a program synthesis framework
built in Racket on the [Rosette](http://emina.github.io/rosette/) library, to
generate sequences of bitvector operations that satisfy the input/output
assertions. For efficiency reasons, we use a relatively low number of samples
from our measured input/output data to generate assertions initially, and then
check the resulting programs against the entire set of data so that we can add
samples that provide counterexamples if necessary (an approach similar to
counterexample-guided synthesis).

We can synthesize both the programs to calculate the direct results of CPU
operations and the programs to calculate the resulting status-register flags by
decomposing the status register from the data set into its component bits and
running synthesis on each one separately. 

3. Emulation and Cosimulation

The resulting synthesized bitvector programs can be used almost directly as
Racket code in the emulator, with some modification to reformat them out of SSA
form and replace references to flag bits with actual extraction of the bits from
the status register. A reasonably small hand-written interface can then dispatch
to these programs based on the opcode of a binary assembly instruction.

The correctness of the emulator can be verified by running programs
simultaneously on both the emulator and on the target hardware. Since the
behavior of the emulator is produced by direct observation of the hardware,
however, it is correct by construction.

## Usage

### Installation

While the synthesis portion of the pipeline and the emulator itself are
platform-independent, the scripts to run them via the command line, 
measurement via the debug interface, and the cosimulation
portion of the emulator presently only work correctly on Linux.

On a machine with a Linux-based operating system, install TI's MSPGCC compiler
toolchain as well as the open-source [MSPDebug](https://github.com/dlbeer/mspdebug) 
interface. You may need to build MSPDebug from source with the following
modification in `formats/elf32.c` on line 35:

    #define MAX_PHDRS	256
    #define MAX_SHDRS	256

This will enable mspdebug to load elf files with more than 32 p-headers, which
are often produced by mspgcc.

You will also need to install two versions of racket: v6.2 and a version at
least as recent as v6.6 (the code has been successfully run on v6.8 - v6.10).
The more recent version should be the system installation, and v6.2 should be
installed to an easily accessible directory (the code assumes ~/racket-6.2 but
this is configurable). Racket v6.2 is available [here](https://download.racket-lang.org/racket-v6.2.html)
and you should be able to acquire a recent version of racket via your package
manager.

Using the v6.2 installation of `raco`, install v1.0 of `rosette` by downloading
[the packaged release](https://github.com/emina/rosette/releases/tag/v1.0) and
running in the extracted folder:

    raco pkg install

Using the more recent version of `raco`, install v2.2 of `rosette` by running

    raco pkg install rosette 

Now clone this repository to an easily accessible directory:

    git clone --recursive https://github.com/billzorn/SAT

### Running

The entire pipeline can be run from the command line using the `main.rkt`
script. This script takes the following arguments:

    -m, -s, -e: whether to run measurement, synthesis, and emulation respectively
    -d: path where measurement data should be stored, if `data/` in the current 
        directory is not desirable
    -o: path where synthesis results should be stored, if `emu/<cpu>/synthesized.rkt`
        in the current directory is not desirable

Additionally, each stage of the pipeline can be run separately via the
`meas/run.rkt`, `synth/run.rkt`, and `emu/run.rkt` scripts if more configuration
is required. 
