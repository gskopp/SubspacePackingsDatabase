# etfcheck — standalone verifier (independent of the apparatus)

    g++ -O2 -std=c++17 etfcheck.cpp -o etfcheck
    ./etfcheck out/     # PASS/FAIL per .gos frame, with reasons

Checks unit columns, equiangularity (= Welch), tightness (G^2=(n/d)G), and trip
count vs filename. Pure C++, no CUDA/LAPACK. This is the one piece that has been
actually compiled and tested.
