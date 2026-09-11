# ETF Apparatus

One program for generating, exactifying, validating, and exporting (d,2d) equiangular
tight frames. Three generation methods behind one CLI, GPU-accelerated, with a
Mathematica round-trip for exact certification of the non-Paley families.

## Pipeline (numbered = source order)

```
01_toolbox     types (Mat), Welch bound, coherence, filename parsing
02_io          the four label-doc writers + ONE shared export_all
03_gen_altproj GPU  alternating projection            ] three generators,
04_gen_minimize GPU batched-restart potential descent ] pick with a flag
05_gen_paley   GPU  closed-form Paley conference ETF   ]
06_convert     SO<->GM<->TP<->slice<->LUT<->GoS
                 - 06_convert_cpu.cpp : cheap conversions + CPU n^3 tensor
                 - 06_convert_gpu.cu  : GPU n^3 tensor (Paley bulk)
07_invariants  distinctTP, moments, nondiagonal moments
08_exactify    Mathematica round-trip (exactify + validate the FRAME)
09_equiv       projective-unitary-permutation equivalence (OpenMP)
10_validate    ETF checks (coherence=Welch, G^2=(n/d)G, trip count)
main.cpp       dispatch
```

## Data flow

**Paley** (guaranteed construction — no validation needed):
```
generate(GPU) -> convert(GPU) -> export .gos/.tp/.exa/.inv        [all C++]
```

**altproj / minimize** (approximate — needs exact certification):
```
C++:         generate(GPU, approx) -> write numeric frame handoff
Mathematica: exactify frame -> VALIDATE frame -> write frame back numeric
C++:         read frame -> build tensor -> export all four files at once
```
Mathematica only does what C++ can't (exactify, validate), and only on the small
FRAME. The heavy n^3 tensor build and all file writing stay in C++. If validation
fails, nothing is exported.

## Build

Requires: CUDA Toolkit, LAPACK/BLAS, OpenMP-capable compiler. For the non-Paley
exact/validate step at runtime: `wolframscript` on PATH (Mathematica installed).

```
# set your GPU arch in CMakeLists.txt (CMAKE_CUDA_ARCHITECTURES):
#   70 V100 | 80 A100 | 86 RTX30xx | 89 RTX40xx | 90 H100
cmake -B build
cmake --build build
```

`exactify_validate.wl` needs the project packages beside it at runtime:
`convertFrameData.wl`, `exactification.wl`, `frameInvariants.wl`. Put them next to
the binary (or edit the Get[] paths in the script).

## Run

```
# Paley family member by prime q (= 1 mod 4). d=q, n=2q. Both signs -> two frames.
./build/etf generate --paley -q 17

# any (d,n) by GPU alternating projection, then Mathematica exact/validate
./build/etf generate --altproj -d 4 -n 8 [--batch 256] [--maxit 30000]

# any (d,n) by GPU batched-restart minimization, then Mathematica exact/validate
./build/etf generate --minimize -d 4 -n 8 [--batch 256]

# validate exported files
./build/etf validate "out/etf_*"

# test equivalence of two frames
./build/etf equiv out/A.gos out/B.gos
```

Outputs land in `out/` as `etf_{d}x{n}_{trip}{alpha}.{gos,tp,exa,inv}`.
`alpha` is provisional (a/b); run `equiv` against your database to finalize it.

## Hardware split (why each piece is where it is)

- Generation: all three GPU (altproj/minimize batch across seeds; Paley assembles on device).
- Convert: GPU for Paley (bulk, large n), CPU for altproj/minimize (one-off). main.cpp picks.
- Exactify + validate: Mathematica (RootApproximant has no C++ equivalent), frame only.
- Everything else (invariants, equiv, validate-check, export): CPU.

## Known limits

- `.tp`/`.exa` hold high-precision NUMERIC triple products, not algebraic expressions.
  The exact algebraic form lives in Mathematica; the returned frame is numeric by design.
- GPU minimize keeps the frame in shared memory (2*d*n complex/block); very large
  (d,n) will exceed shared memory and need a global-memory variant.
- cusolverDnZheevjBatched signature shifts across CUDA versions (altproj) — check on build.
- Paley alpha a/b is provisional until equiv-vs-database assigns real letters.

## Deployment — what travels with the binary

Compiling bakes the C++ into the `etf` binary. The Wolfram files do NOT compile in;
they are read at runtime by wolframscript. They must sit next to the binary.

CMake copies all four next to the binary automatically at build time:
  exactify_validate.wl   (the driver)
  convertFrameData.wl    (from your project)
  exactification.wl      (from your project)
  frameInvariants.wl     (from your project)

The binary locates them relative to its own path (/proc/self/exe), so it works from
any working directory as long as the four .wl files are beside the executable.

- Paley-only runs need NONE of this — pure binary, no Wolfram, no Mathematica.
- altproj/minimize runs need the four .wl files beside the binary AND wolframscript
  (Mathematica) on the machine. The .wl files are data, not compiled code.

The driver passes d and n to Mathematica on the command line, so it does not depend
on any filename-parsing helper from the original packages.
