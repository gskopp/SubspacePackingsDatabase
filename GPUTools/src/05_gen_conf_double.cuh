// 05_gen_conf_double.cuh — doubled-conference-graph (d, 2d) ETF generator.
//
// This is the construction of Iverson-Jasper-Mixon, "More on the optimal
// arrangement of 2d lines in C^d" (arXiv:2410.17379), Theorem 8: doubling a
// conference graph on v vertices yields a complex v x 2v ETF. Applied to the
// Paley graph on q vertices (q prime, q = 1 mod 4) this gives the paper's
// 2.Gq family with d = q, n = 2q.
//
// NOTE ON NAMING: earlier revisions called this the "paley" method. That was a
// misnomer -- the classical Paley construction is the order-(q+1) conference
// matrix G_q + 1 giving a (q+1)/2 x (q+1) ETF, which is a different object.
// What we build here is the *doubled conference graph* of Theorem 8. Hence
// "conf_double".
//
// The signature-matrix blocks (eq. (4) of the paper), with A = Paley-graph
// adjacency, B = J - I - A the complement, epsilon in {+1,-1}, x, y, beta as
// defined below, are assembled on the GPU and G = I + gamma*S is returned
// host-side. Closed form -- no search, no iteration.
#pragma once
#include "01_toolbox.hpp"

namespace etf {

// Assemble the 2q x 2q Gram matrix on the GPU and return it host-side.
// eps in {+1,-1}. Throws std::runtime_error on bad q (not prime, not = 1 mod 4).
Mat conf_double_gram(int q, int eps);

// convenience: is q a legal parameter here?
// Requires q prime and q = 1 mod 4 (primality -> Paley graph is circulant ->
// the resulting ETF is 2-circulant, per Thm 28 of the paper).
bool conf_double_valid(int q);

} // namespace etf
