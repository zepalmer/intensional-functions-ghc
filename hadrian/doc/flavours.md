# Build flavours

Hadrian supports a few predefined _build flavours_, i.e. collections of build
settings that fully define a GHC build (see `src/Flavour.hs`). Users can add their
own build flavours if need be, as described
[here](https://gitlab.haskell.org/ghc/ghc/blob/master/hadrian/doc/user-settings.md#build-flavour).

## Arguments

The following table summarises extra arguments passed to GHC in different build flavours.
There are four groups of arguments: arguments in `hsDefault` are passed to GHC for all Haskell
source files, `hsLibrary` arguments are added when compiling libraries, `hsCompiler`
when compiling the `compiler` library, and `hsGhc` when compiling/linking the GHC program.

<table>
  <tr>
    <th rowspan="3">Flavour</th>
    <th colspan="8">Extra arguments</th>
  </tr>
  <tr>
    <th colspan="2">hsDefault</td>
    <th colspan="2">hsLibrary</td>
    <th colspan="2">hsCompiler</td>
    <th colspan="2">hsGhc</td>
  </tr>
  <tr>
    <th>stage0</td>
    <th>stage1+</td>
    <th>stage0</td>
    <th>stage1+</td>
    <th>stage0</td>
    <th>stage1+</td>
    <th>stage0</td>
    <th>stage1+</td>
  </tr>
  <tr>
    <th>default<br></td>
    <td>-O<br>-H32m<br></td>
    <td>-O2<br>-H32m</td>
    <td></td>
    <td>-haddock</td>
    <td></td>
    <td></td>
    <td></td>
    <td></td>
  </tr>
  <tr>
    <th>quick</td>
    <td>-O0<br>-H64m</td>
    <td>-O0<br>-H64m</td>
    <td></td>
    <td>-O</td>
    <td>-O2</td>
    <td>-O</td>
    <td>-O</td>
    <td></td>
  </tr>
  <tr>
    <th>quick-validate</td>
    <td>-O0<br>-H64m<br>-Werror</td>
    <td>-O0<br>-H64m<br>-Werror</td>
    <td></td>
    <td>-O</td>
    <td>-O2</td>
    <td>-O</td>
    <td>-O</td>
    <td></td>
  </tr>
  <tr>
    <th>quick-debug</td>
    <td>-O0<br>-H64m</td>
    <td>-O0<br>-H64m</td>
    <td></td>
    <td>-O</td>
    <td>-O2</td>
    <td>-O</td>
    <td>-O</td>
    <td>-debug (link)</td>
  </tr>
  <tr>
    <th>quickest</td>
    <td>-O0<br>-H64m</td>
    <td>-O0<br>-H64m</td>
    <td></td>
    <td></td>
    <td>-O</td>
    <td></td>
    <td>-O</td>
    <td></td>
  </tr>
  <tr>
    <th>perf</td>
    <td>-O<br>-H64m</td>
    <td>-O<br>-H64m</td>
    <td></td>
    <td>-O2</td>
    <td>-O2</td>
    <td>-O2</td>
    <td>-O</td>
    <td>-O2</td>
  </tr>
  <tr>
    <th>bench</td>
    <td>-O<br>-H64m</td>
    <td>-O<br>-H64m</td>
    <td></td>
    <td>-O2</td>
    <td>-O2</td>
    <td>-O0</td>
    <td>-O2</td>
    <td>-O2</td>
  </tr>
  <tr>
    <th>devel1</td>
    <td>-O<br>-H64m</td>
    <td>-O<br>-H64m</td>
    <td></td>
    <td>-dcore-lint</td>
    <td>-O0<br>-DDEBUG</td>
    <td></td>
    <td>-O0<br>-DDEBUG</td>
    <td></td>
  </tr>
  <tr>
    <th>devel2</td>
    <td>-O<br>-H64m</td>
    <td>-O<br>-H64m</td>
    <td></td>
    <td>-dcore-lint</td>
    <td>-O2</td>
    <td>-O0<br>-DDEBUG</td>
    <td></td>
    <td>-O0<br>-DDEBUG</td>
  </tr>
  <tr>
    <th>validate</td>
    <td>-O0<br>-H64m</td>
    <td>-fllvm-fill-undef-with-garbage</td>
    <td></td>
    <td>-O<br>-dcore-lint<br>-dno-debug-output</td>
    <td>-O2<br>-DDEBUG</td>
    <td>-O<br>-dcore-lint<br>-dno-debug-output</td>
    <td>-O</td>
    <td>-O</td>
  </tr>
  <tr>
    <th>slow-validate</td>
    <td>-O0<br>-H64m</td>
    <td>-fllvm-fill-undef-with-garbage</td>
    <td></td>
    <td>-O<br>-dcore-lint<br>-dno-debug-output</td>
    <td>-O2<br>-DDEBUG</td>
    <td>-O<br>-DDEBUG<br>-dcore-lint<br>-dno-debug-output</td>
    <td>-O</td>
    <td>-O</td>
  </tr>
  <tr>
    <th>static</td>
    <td>-O<br>-H64m<br>-fPIC -static</td>
    <td>-O<br>-H64m<br>-fPIC -static</td>
    <td></td>
    <td>-O2</td>
    <td>-O2</td>
    <td>-O2</td>
    <td>-O<br>-optl -static</td>
    <td>-O2<br>-optl -static</td>
  </tr>
</table>

## Flavour transformers

Each of the flavours described above is intended as a starting-point for
configuring your GHC build. In addition, Hadrian supports a number of "flavour
transformers" which modify the configuration in various ways.

These can be appended to the flavour name passed via the `--flavour`
command-line flag, separated by the `+` character. For instance,

```
hadrian --flavour=perf+thread_sanitizer
```

The supported transformers are listed below:

<table>
    <tr>
        <th>Transformer name</th>
        <th>Effect</th>
    </tr>
    <tr>
        <td><code>werror</code></td>
        <td>Use the `-Werror` flag for all stage1+ compilation.</td>
    </tr>
    <tr>
        <td><code>debug_info</code></td>
        <td>Enable production of native debugging information (via GHC/GCC's `-g3`)
            during stage1+ compilations.</td>
    </tr>
    <tr>
        <td><code>ticky_ghc</code></td>
        <td>Compile the GHC executable with Ticky-Ticky profiler support.</td>
    </tr>
    <tr>
        <td><code>split_sections</code></td>
        <td>Enable section splitting for all libraries (except for the GHC
            library due to the long linking times that this causes).</td>
    </tr>
    <tr>
        <td><code>thread_sanitizer</code></td>
        <td>Build the runtime system with ThreadSanitizer support</td>
    </tr>
    <tr>
        <td><code>llvm</code></td>
        <td>Use GHC's LLVM backend (`-fllvm`) for all stage1+ compilation.</td>
    </tr>
    <tr>
        <td><code>profiled_ghc</code></td>
        <td>Build the GHC executable with cost-centre profiling support.
            It is recommended that you use this in conjunction with `no_dynamic_ghc` since
            GHC does not support loading of profiled libraries with the
            dynamic linker. You should use a flavour that builds profiling libs and rts,
            i.e. not <code>quick</code>.</td>
    </tr>
    <tr>
        <td><code>no_dynamic_ghc</code></td>
        <td>Linked GHC against the statically-linked RTS. This causes GHC to
            default to loading static rather than dynamic library when,
            e.g., loading libraries during TemplateHaskell evaluations.</td>
    </tr>
    <tr>
        <td><code>no_profiled_libs</code></td>
        <td>Disables building of libraries in profiled build ways.</td>
    </tr>
    <tr>
        <td><code>omit_pragmas</code></td>
        <td>Build the stage2 compiler with -fomit-interface-pragmas to reduce
        recompilation.</td>
    </tr>
    <tr>
        <td><code>ipe</code></td>
        <td>Build the stage2 libraries with IPE debugging information for use with -hi profiling.</td>
    </tr>
</table>

### Static

The `static` flavour does not strictly follow the groupings in the table
above because it links all the executables statically, not just GHC
itself, and because it passes `-optc -static` when delegating to a C
compiler.  It also adds the `-dynamic-system-linker` cabal flag to the
`ghc` package build and only adds static flags when building in a
non-dynamic _way_.  Some of the considerations for a static build aren't
a great fit for the flavour system, so it's a little bit hacky.

## Ways

Libraries and GHC can be built in different _ways_, e.g. with or without profiling
information. The following table lists ways that are built in different flavours.

<table>
    <tr>
        <th rowspan="2">Flavour</th>
        <th colspan="2">Library ways</th>
        <th colspan="2">RTS ways</th>
    </tr>
    <tr>
        <th>stage0</th>
        <th>stage1+</th>
        <th>stage0</th>
        <th>stage1+</th>
        <th>stage0</th>
        <th>stage1+</th>
    </tr>
    <tr>
    <th>default<br>perf<br>prof<br>devel1<br>devel2</td>
    <td>vanilla</td>
    <td>vanilla<br>profiling<br>dynamic</td>
    <td>logging<br>debug<br>threaded<br>threadedDebug<br>threadedLogging
        <br>debugDynamic<br>threadedDynamic<br>threadedDebugDynamic
        <br>loggingDynamic<br>threadedLoggingDynamic
    </td>
    <td>
        logging<br>debug<br>threaded<br>threadedDebug<br>
        threadedLogging<br>threadedProfiling
        <br>debugDynamic<br>threadedDynamic<br>threadedDebugDynamic
        <br>loggingDynamic<br>threadedLoggingDynamic
    </td>
</tr>
<tr>
    <th>static</td>
    <td>vanilla</td>
    <td>vanilla<br>profiling</td>
    <td>logging<br>debug<br>threaded<br>threadedDebug<br>threadedLogging
    </td>
    <td>
        logging<br>debug<br>threaded<br>threadedDebug<br>
        threadedLogging<br>threadedProfiling
    </td>
    <td>Only in<br>prof<br>flavour</td>
    <td>Only in<br>prof<br>flavour</td>
</tr>
<tr>
    <th>quick<br>quick-validate<br>quick-debug</th>
    <td>vanilla</td>
    <td>vanilla<br>dynamic</td>
    <td>logging<br>debug<br>threaded<br>threadedDebug<br>threadedLogging
        <br>debugDynamic<br>threadedDynamic<br>threadedDebugDynamic
        <br>loggingDynamic<br>threadedLoggingDynamic
    </td>
    <td>logging<br>debug<br>threaded<br>threadedDebug<br>threadedLogging
        <br>debugDynamic<br>threadedDynamic<br>threadedDebugDynamic
        <br>loggingDynamic<br>threadedLoggingDynamic
    </td>
</tr>
<tr>
    <th>quickest<br>bench</th>
    <td>vanilla</td>
    <td>vanilla</td>
    <td>vanilla<br>threaded</td>
    <td>vanilla<br>threaded</td>
</tr>
</table>
