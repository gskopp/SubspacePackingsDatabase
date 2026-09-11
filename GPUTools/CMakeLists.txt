cmake_minimum_required(VERSION 3.24)
project(etf LANGUAGES CXX CUDA)

# ---------------------------------------------------------------------------
# ETF apparatus — CUDA + LAPACK build.
#
#   cmake -B build -DCMAKE_BUILD_TYPE=Release
#   cmake --build build -j
#   ./build/etf generate --conf-double -q 17
#
# GPU arch override (default 89 = RTX40xx; 70 V100, 80 A100, 86 RTX30xx, 90 H100):
#   cmake -B build -DCMAKE_CUDA_ARCHITECTURES=90
#
# The exe drives the whole pipeline. For altproj/minimize it REQUIRES the Wolfram
# round-trip (exactify + validate in Mathematica). The .wl scripts are staged
# next to the binary so the exe finds them regardless of CWD. The requirement is
# compiled in via ETF_REQUIRE_WOLFRAM=1 below.
# ---------------------------------------------------------------------------

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CUDA_STANDARD 17)
set(CMAKE_CUDA_STANDARD_REQUIRED ON)

if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
  set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS Debug Release RelWithDebInfo)
endif()

# GPU arch: 70 V100, 80 A100, 86 RTX30xx, 89 RTX40xx, 90 H100
if(NOT DEFINED CMAKE_CUDA_ARCHITECTURES)
  set(CMAKE_CUDA_ARCHITECTURES 89)
endif()

find_package(CUDAToolkit REQUIRED)   # CUDA::cudart, cusolver, cublas, curand
find_package(LAPACK REQUIRED)        # host zgesdd_ (06_convert_cpu.cpp)
find_package(OpenMP REQUIRED)        # 09_equiv.cpp coset-parallel hashing

# ---------------------------------------------------------------------------
# Sources — MUST match the files actually present in src/.
#   05_gen_conf_double.*  (was 05_gen_paley.* — renamed; see file headers)
#   06_convert_gpu.cu     (GPU n^3 tensor build, conf_double bulk path)
#   11_hash.*             (order-invariant TP hashing; ported from frameEquiv.wl)
# ---------------------------------------------------------------------------
add_executable(etf
  src/01_toolbox.cpp
  src/02_io.cpp
  src/03_gen_altproj.cu        # GPU generator (alternating projections)
  src/04_gen_minimize.cu       # GPU generator (batched restarts)
  src/05_gen_conf_double.cu    # GPU generator (doubled conference graph, closed form)
  src/06_convert_cpu.cpp       # cheap conversions + CPU n^3 tensor + host SVD
  src/06_convert_gpu.cu        # GPU n^3 tensor (conf_double bulk path)
  src/07_invariants.cpp
  src/08_exactify.cpp          # wolframscript subprocess bridge
  src/09_equiv.cpp
  src/10_validate.cpp
  src/11_hash.cpp              # EncodeComplex / HashfunctionH port
  src/main.cpp
)

target_include_directories(etf PRIVATE src)

set_target_properties(etf PROPERTIES
  CUDA_SEPARABLE_COMPILATION ON
  CUDA_RESOLVE_DEVICE_SYMBOLS ON
  POSITION_INDEPENDENT_CODE  ON
)

# ---------------------------------------------------------------------------
# Maximal optimization — host C++ and device CUDA (Release).
# Host flags are compiler-conditional: GNU/Clang get -O3 -march=native etc.;
# MSVC gets /O2 /fp:precise /arch:AVX2. No global fast-math on host: ETF checks
# compare to the Welch bound at ~1e-15 and rely on IEEE rounding. Device kernels
# are assemble/reduce loops where --use_fast_math is acceptable and wanted.
# ---------------------------------------------------------------------------
# GNU/Clang host flag set
set(ETF_HOST_GNU
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>,$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>>:-O3>
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>,$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>>:-march=native>
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>,$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>>:-mtune=native>
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>,$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>>:-funroll-loops>
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>,$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>>:-fno-math-errno>
)
# MSVC host flag set
set(ETF_HOST_MSVC
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>,$<CXX_COMPILER_ID:MSVC>>:/O2>
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>,$<CXX_COMPILER_ID:MSVC>>:/fp:precise>
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>,$<CXX_COMPILER_ID:MSVC>>:/arch:AVX2>
)
# Host flags nvcc passes to its host compiler via -Xcompiler differ per platform.
if(MSVC)
  set(ETF_CUDA_HOST_PASSTHROUGH -Xcompiler=/O2)
else()
  set(ETF_CUDA_HOST_PASSTHROUGH -Xcompiler=-O3,-march=native,-funroll-loops)
endif()

target_compile_options(etf PRIVATE
  # ---- host C++ (both toolchains) ----
  ${ETF_HOST_GNU}
  ${ETF_HOST_MSVC}
  # ---- device CUDA (toolchain-independent) ----
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CUDA>>:-O3>
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CUDA>>:--use_fast_math>
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CUDA>>:--fmad=true>
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CUDA>>:--extended-lambda>
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CUDA>>:--expt-relaxed-constexpr>
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CUDA>>:${ETF_CUDA_HOST_PASSTHROUGH}>
  $<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CUDA>>:-Xptxas=-O3>
  $<$<AND:$<CONFIG:RelWithDebInfo>,$<COMPILE_LANGUAGE:CUDA>>:-lineinfo>
)

target_link_libraries(etf PRIVATE
  CUDA::cudart CUDA::cusolver CUDA::cublas CUDA::curand
  LAPACK::LAPACK OpenMP::OpenMP_CXX)

# ---------------------------------------------------------------------------
# The exe REQUIRES the Wolfram round-trip for altproj/minimize. Compiled-in
# policy: the C++ driver is the authority; a missing/failed wolframscript is a
# hard error (no silent skip). --allow-approx is the explicit per-run opt-out.
# ---------------------------------------------------------------------------
target_compile_definitions(etf PRIVATE ETF_REQUIRE_WOLFRAM=1)

# ---------------------------------------------------------------------------
# Runtime Wolfram files — copied next to the binary so the exe finds them
# regardless of CWD (08_exactify.cpp resolves the exe dir per-platform).
# ---------------------------------------------------------------------------
set(ETF_WL_FILES
  exactify_validate.wl
  convertFrameData.wl
  exactification.wl
  frameInvariants.wl
)
foreach(wl ${ETF_WL_FILES})
  add_custom_command(TARGET etf POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${CMAKE_SOURCE_DIR}/wolfram/${wl}
            $<TARGET_FILE_DIR:etf>/${wl}
    COMMENT "Staging Wolfram script ${wl} next to etf binary")
endforeach()

# ---------------------------------------------------------------------------
# Standalone verifier (tools/etfcheck.cpp): no CUDA/LAPACK, one TU.
# ---------------------------------------------------------------------------
add_executable(etfcheck tools/etfcheck.cpp)
target_compile_options(etfcheck PRIVATE
  $<$<AND:$<CONFIG:Release>,$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>>:-O3>
  $<$<AND:$<CONFIG:Release>,$<OR:$<CXX_COMPILER_ID:GNU>,$<CXX_COMPILER_ID:Clang>>>:-march=native>
  $<$<AND:$<CONFIG:Release>,$<CXX_COMPILER_ID:MSVC>>:/O2>)

message(STATUS "ETF build:")
message(STATUS "  build type        : ${CMAKE_BUILD_TYPE}")
message(STATUS "  CUDA architectures: ${CMAKE_CUDA_ARCHITECTURES}")
message(STATUS "  CUDA toolkit      : ${CUDAToolkit_VERSION}")
message(STATUS "  LAPACK            : ${LAPACK_LIBRARIES}")
message(STATUS "  Wolfram required  : ETF_REQUIRE_WOLFRAM=1 (altproj/minimize)")
