***********************************
LABEL AND DATA CONVENTIONS FOR ETFs
UNDER CONSTRUCTION
***********************************
An ETF is considered "isolated" if there exists an open neighborhood of it that does not contain any ETF that it is not unitarily equivalent to, and "nonisolated" otherwise.

An ETF label for an isolated ETF is of the form "etf_dxn_alpha_trip", where d, n, alpha, and trip will be replaced by certain data.
d = dimension
n = number of vectors
alpha = a lowercase letter or sequence of lowercase letters, uniquely specifying the ETF among those with parameters (d,n)
trip = the number of distinct triple products, including the two degenerate triple products

The labels alpha for isolated ETFs with parameters (d,n) are chosen in order from {a, b, c, ..., z, aa, ab, ...} using the following ordering on projective permutation unitary equivalence classes of ETFs.
    1. ETF_1 < ETF_2 if trip(ETF_1) < trip(ETF_2).
    2. If trip(ETF_1) = trip(ETF_2), then ETF_1 < ETF_2 if the first unequal moment mu_m(ETF_1) < mu_m(ETF_2).
    2. If all moments are equal, then the ordering is determined arbitrarily.

An ETF label for a nonisolated ETF is of the form "etf_dxn_ALPHAdim_trip", where d, n, ALPHA, and trip will be replaced by certain data.
d = dimension
n = number of vectors
ALPHA = an uppercase letter or sequence of uppercase letters
dim = the dimension of the irreducible component of the ETF in the real algebraic "variety" of projective permutation unitary equivalence classes of ETFs
trip = the number of distinct triple products, including the two degenerate triple products

An ETF label for a nonisolated ETF represents not a single projective unitary equivalence class of ETFs, but the irreducible component of that class in the real algebraic "variety" of projective permutation unitary equivalence classes of ETFs. The representative ETF is assumed to be a generic point on the component. The word "variety" is in scare quotes because this object is not really a variety. Tentatively, it is the GIT quotient of the real algebraic variety of (d,n)-ETFs by the action of the product of the unitary group U(d) and the group of nxn generalized permutation matrices.

The labels ALPHA for nonisolated ETFs with parameters (d,n) are chosen in order from {A, B, C, ..., Z, AA, AB, ...} using the following ordering.
    1. ETF_1 < ETF_2 if dim(ETF_1) < dim(ETF_2).
    2. If dim(ETF_1) = dim(ETF_2), then ETF_1 < ETF_2 if trip(ETF_1) < trip(ETF_2). (Note that the number of triple products is constant on a Zariski open subset of any component.)
    3. If the component dimensions and number of triple products are the equal, then the ordering is determined arbitrarily.

Note that ETF labels are subject to change, for the following two reasons.
    1. If all (d,n)-ETFs have not been discovered, the discovery of new (d,n)-ETFs will likely require relabelling previously known (d,n)-ETFs.
    2. The labelling scheme specified here is itself tentative and may be subject to breaking changes. For example, we may decide to introduce labels for nongeneric nonisolated ETFs, such as those with larger stabalizer groups, those at singular points of components, or those at intersection points of two or more components.

Files associated to an ETF label are as follows.
    1. "eft_dxn_alpha_trip.gos" or "etf_dxn_ALPHAdim_trip.gos" is a text file consisting of a list of real numbers that specifies the ETF numerically in the Game of Sloanes format.
    2. "eft_dxn_alpha_trip.exa" or "etf_dxn_ALPHAdim_trip.exa" is a text file consisting of data specifying the ETF exactly. The structure of this file is TBD.
    3. "eft_dxn_alpha_trip.inv" or "etf_dxn_ALPHAdim_trip.inv" is a text file specifying certain invariants of the ETF. The structure of this file is TBD.
