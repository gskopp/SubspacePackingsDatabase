# Building on Windows

The code is cross-platform (MSVC + CUDA). You need three things: the CUDA
Toolkit, a LAPACK implementation, and CMake. Below is the path that works with
the least friction.

## 1. Prerequisites

- **Visual Studio** with the "Desktop development with C++" workload
  (gives you the MSVC `cl.exe` host compiler CUDA needs).
- **CUDA Toolkit 12.x** for Windows (installs `nvcc` and the cuSOLVER/cuRAND/
  cuBLAS libraries; the installer registers the VS integration).
- **CMake 3.24+** (bundled with recent VS, or from cmake.org).
- **LAPACK** — this is the only non-obvious dependency on Windows. Pick ONE:

### LAPACK option A — vcpkg (recommended)
```
git clone https://github.com/microsoft/vcpkg
.\vcpkg\bootstrap-vcpkg.bat
.\vcpkg\vcpkg install lapack openblas
```
Then configure with the vcpkg toolchain (see step 2).

### LAPACK option B — Intel oneMKL
Install oneMKL; it provides LAPACK. CMake's `find_package(LAPACK)` locates it if
`MKLROOT` is set. Heavier, but the fastest LAPACK on Intel CPUs.

## 2. Configure + build

From a **"x64 Native Tools Command Prompt for VS"** (so `cl.exe` is on PATH):

### With vcpkg:
```
cmake -B build -G "Visual Studio 17 2022" -A x64 ^
  -DCMAKE_BUILD_TYPE=Release ^
  -DCMAKE_TOOLCHAIN_FILE=C:/path/to/vcpkg/scripts/buildsystems/vcpkg.cmake ^
  -DCMAKE_CUDA_ARCHITECTURES=89
cmake --build build --config Release -j
```

### Without vcpkg (MKL or a LAPACK you installed manually):
```
cmake -B build -G "Visual Studio 17 2022" -A x64 ^
  -DCMAKE_BUILD_TYPE=Release -DCMAKE_CUDA_ARCHITECTURES=89
cmake --build build --config Release -j
```

Set `-DCMAKE_CUDA_ARCHITECTURES` to YOUR GPU: 89 RTX40xx, 86 RTX30xx,
80 A100, 90 H100, 70 V100.

The binary lands in `build\Release\etf.exe`, with the four `.wl` scripts staged
next to it automatically.

## 3. Run

```
build\Release\etf.exe generate --conf-double -q 17
build\Release\etf.exe generate --altproj -d 3 -n 6
build\Release\etf.exe validate out\*
```

`--conf-double` needs no Mathematica. `--altproj` / `--minimize` REQUIRE
`wolframscript` (Wolfram Engine or Mathematica) on PATH; the exe hard-fails
without it unless you pass `--allow-approx`. On Windows the exe finds
`wolframscript.exe` via the `where` command and locates the `.wl` scripts next
to `etf.exe`.

## Windows-specific notes baked into the code
- `08_exactify.cpp` uses `GetModuleFileNameA` (not `/proc/self/exe`) to find the
  exe directory, and `where` (not `command -v`) to probe for wolframscript.
- `main.cpp` uses `std::filesystem::create_directories` (not `mkdir -p`).
- CMake feeds MSVC `/O2 /fp:precise /arch:AVX2` instead of the GCC `-O3
  -march=native` flags; the CUDA device flags are the same on both platforms.

## If CMake can't find LAPACK
`find_package(LAPACK REQUIRED)` fails if no LAPACK is discoverable. Either use
the vcpkg toolchain file (option A), or point CMake at your install with
`-DLAPACK_ROOT=...` / `-DBLAS_ROOT=...`, or set `MKLROOT` for oneMKL.
