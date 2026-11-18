(* ::Package:: *)

Clear[PhiVarGen];
(* Normalize columns of a matrix *)
normalizeCols[m_] := Map[#/Norm[#] &, m];
(* Generate symbolic variable matrix PhiVar based on d and n *)
Clear[PhiVarGen];
PhiVarGen[d_, n_] := Table[a[i, j] + b[i, j] I, {i, 1, n}, {j, 1, d}];
(* List of initial values for variables *)
varcons[Phi0_, d_, n_] := Join[
  Transpose[{Flatten[Table[a[i, j], {i, 1, n}, {j, 1, d}]], Re[Flatten[Phi0]]}],
  Transpose[{Flatten[Table[b[i, j], {i, 1, n}, {j, 1, d}]], Im[Flatten[Phi0]]}]];
(* Coherence of a matrix *)
Coherence[Phi_] := Max[Flatten[Table[Abs[Phi[[i]]\[Conjugate] . Phi[[j]]] / (Norm[Phi[[i]]]*Norm[Phi[[j]]]), {i, 1, Length[Phi]}, {j, i + 1, Length[Phi]} ]]];

(* p-frame potential of a matrix *)
pFramePotential[Phi_, p_] := Sum[
  (Abs[Phi[[i]]\[Conjugate] . Phi[[j]]] / (Norm[Phi[[i]]]*Norm[Phi[[j]]]))^p,{i, 1, Length[Phi]}, {j, i + 1, Length[Phi]}];

(* Welch bound *)
Welch[d_, n_] := Sqrt[(n - d)/(d (n - 1))];

(* Random complex n x d initialization *)
rand[d_, n_] := RandomReal[NormalDistribution[], {n, d}] + I RandomReal[NormalDistribution[], {n, d}];

(* Minimize frame potential with quasi-Newton *)
MinPhiQNp[Phi0_, p_, NN_] := Module[
  {n = Length[Phi0], d = Length[Phi0[[1]]], PhiVar},
  PhiVar = PhiVarGen[d, n];
  FindMinimum[pFramePotential[normalizeCols[PhiVar], p], varcons[Phi0, d, n],
  Method -> "QuasiNewton",MaxIterations -> 1000,WorkingPrecision -> NN]];

(*Main ETF search:produce one ETF*)
ETFSearch[d_, n_] := Module[
  {min, comin, min1, co1, min40, min80, min160, min320, ETF, etfcheck, i, PhiVar},
  While[True,
    (* Retry loop *)
    (* Declare fresh symbolic optimization variables *)
    PhiVar = PhiVarGen[d, n];
    min = {}; comin = Infinity;
    (* Inner loop: multiple random starts, pick lowest coherence *)
    For[i = 1, i <= 10, i++,
      min1 = MinPhiQNp[rand[d, n], 4, 10];
      co1 = Coherence[normalizeCols[PhiVar /. min1[[2]]]];
      If[co1 < comin, min = min1; comin = co1];
    ];
    (* Refinement steps *)
    min40 = MinPhiQNp[normalizeCols[PhiVar /. min[[2]]], 4, 40];
    min80 = MinPhiQNp[normalizeCols[PhiVar /. min40[[2]]], 4, 80];
    min160 = MinPhiQNp[normalizeCols[PhiVar /. min80[[2]]], 4, 160];
    min320 = MinPhiQNp[normalizeCols[PhiVar /. min160[[2]]], 4, 320];
    
    ETF = normalizeCols[PhiVar /. min320[[2]]];
    etfcheck = (Coherence[ETF] - Welch[d, n]) < 10^-20;
    
    If[etfcheck, Return[ETF]]; (* Success \[RightArrow] return ETF *)]];


(* Convention (Bach-Sandlund): right action Phi P_sigma, row i -> v_sigma(i).
   Collision Phi P_t = Psi P_h, t in T, h in H; isomorphism g = t h^-1. *)

(* Right action: plain gather. (Inverse removed: old version was the left action.) *)
PermuteRows[etf_?MatrixQ, sigma_] := Module[{n = Length[etf], permList},
  permList = PermutationList[sigma, n];
  etf[[permList]]]

HermitianInnerProduct[v1_?VectorQ, v2_?VectorQ] := Conjugate[v1] . v2;

ComputeHermitianTripleProducts[etf_?MatrixQ, precision_: 40] :=
 Module[{n = Length[etf]},
  If[n == 0, Return[{}]];
  If[Length[etf[[1]]] == 0, Return[{N[0, precision]}]];
  Table[N[HermitianInnerProduct[etf[[i]], etf[[j]]]*
     HermitianInnerProduct[etf[[j]], etf[[k]]]*
     HermitianInnerProduct[etf[[k]], etf[[i]]], precision],
   {i, n}, {j, n}, {k, n}]]

(* Tensor of Phi P_sigma by index gather on the stored tensor: no inner products. *)
PermuteTensor[tensor_, sigma_, n_] :=
  With[{p = PermutationList[sigma, n]}, tensor[[p, p, p]]];

FindOptimalKL[n_Integer] := Module[{sqrtNFact, bestK = 0, bestL = 0,
   minDiff = Infinity, kMax, localHSize, currentDiff, k, l},
  sqrtNFact = Sqrt[N[Factorial[n]]];
  If[n <= 1, Return[{0, 0}]];
  kMax = Min[n, n - 2];
  While[kMax > 0 && Factorial[kMax] > 2*sqrtNFact && kMax > 1, kMax--];
  If[kMax == 0, kMax = 1];
  For[k = 0, k <= kMax, k++,
   For[l = 0, l <= n - k, l++,
    If[k == 0 && l == 0, Continue[]];
    localHSize = If[l == 0 || l == 1, N[Factorial[k]], N[l*Factorial[k]]];
    If[localHSize == 0, Continue[]];
    currentDiff = Abs[localHSize - sqrtNFact];
    If[currentDiff < minDiff, minDiff = currentDiff; bestK = k; bestL = l]]];
  {bestK, bestL}]

GenerateSubgroupH[n_, k_, l_] := Block[{harvested},
  If[k + l > n || (k + l) == 0, Return[{}]];
  harvested = Reap[Which[
      k == 0, If[l >= 2, Sow[Cycles[{Range[1, l]}]]],
      k == 1, If[l >= 2, Sow[Cycles[{Range[2, l + 1]}]]],
      k >= 2, (Sow[Cycles[{{1, 2}}]]; Sow[Cycles[{Range[1, k]}]];
        If[l >= 2, Sow[Cycles[{Range[k + 1, k + l]}]]])]][[2, 1]];
  harvested]

GenerateTransversalH2[n_, k_, l_] := Module[
  {A, B, Cc, permsB, permsC, patterns, posIdx, build, harvested},
  If[k + l > n || k + l == 0, Return[{}]];
  A  = Range[1, k];
  B  = Range[k + 1, k + l];
  Cc = Range[k + l + 1, n];

  permsB = If[Length[B] <= 1, {B},
              Prepend[#, First[B]] & /@ Permutations[Rest[B]]];
  permsC = If[Length[Cc] == 0, {{}}, Permutations[Cc]];

  (* interleaving patterns depend ONLY on the block sizes -> build once.
     Permutations of a multiset returns exactly n!/(k! l! (n-k-l)!) items,
     with no 3^n enumeration. *)
  patterns = Permutations[
     Join[ConstantArray[1, k], ConstantArray[2, l],
          ConstantArray[3, n - k - l]]];

  (* precompute, per pattern, the slot indices for the A/B/C blocks *)
  posIdx = {Flatten@Position[#, 1], Flatten@Position[#, 2],
            Flatten@Position[#, 3]} & /@ patterns;

  (* assemble one transversal element by scatter-assignment (vectorized) *)
  build[b_, c_, {pa_, pb_, pc_}] := Module[{out = ConstantArray[0, n]},
     out[[pa]] = A; out[[pb]] = b; out[[pc]] = c; out];

  harvested = Flatten[
     Table[build[b, c, p], {b, permsB}, {c, permsC}, {p, posIdx}], 2];

  PermutationCycles /@ harvested];
  (*Correcting Generate Transversal H nested multiple loop of 3^n*)
  
(* Quantise to tol grid, fold Re/Im into one integer. (Old 10^-20 scale is
   below machine precision; tol now a parameter, default 10^-8.) *)
EncodeComplex[z_, tol_: 10^-8] :=
  Round[Re[z]/tol]*2^31 + Round[Im[z]/tol];

(* Order-sensitive polynomial hash: permuting entries changes the digest. *)
HashfunctionH[keyvec_List, tol_: 10^-8] :=
  With[{prime = 2305843009213693951, base = 1299721},
   Fold[Mod[#1 base + #2, prime] &, 0,
    Mod[EncodeComplex[#, tol], prime] & /@ keyvec]];

Matchlist[list1_, list2_, verbose_: False] := Module[{tagged1, tagged2,
   sorted1, sorted2, i = 1, j = 1, matches = {}},
  tagged1 = MapIndexed[{#1, First[#2]} &, list1];
  tagged2 = MapIndexed[{#1, First[#2]} &, list2];
  sorted1 = SortBy[tagged1, First];
  sorted2 = SortBy[tagged2, First];
  While[i <= Length[sorted1] && j <= Length[sorted2],
   With[{val1 = First[sorted1[[i]]], pos1 = Last[sorted1[[i]]],
     val2 = First[sorted2[[j]]], pos2 = Last[sorted2[[j]]]},
    Which[
     val1 == val2, AppendTo[matches, {val1, {pos1, pos2}}]; i++; j++,
     val1 < val2, i++,
     True, j++]]];
  If[matches === {}, False,
   If[verbose, Column[{True, "Collisions found:",
      TableForm[matches,
       TableHeadings -> {None, {"value", "{pos in list1, pos in list2}"}}]}],
    matches]]]

OptimalKLCache[n_] := OptimalKLCache[n] = FindOptimalKL[n];
hGeneratorsCyclesCache[n_, k_, l_] :=
  hGeneratorsCyclesCache[n, k, l] = GenerateSubgroupH[n, k, l];
GenerateTransversalHCache[n_, k_, l_] :=
  GenerateTransversalHCache[n, k, l] = GenerateTransversalH2[n, k, l];

(* Left-transversal guarantee: S_n = union of t H. Inverts a cached right
   transversal; $Failed if T does not match H. *)
EnsureLeftTransversal[tElems_, hElems_, n_] := Module[{tL, hL, cover},
  tL = PermutationList[#, n] & /@ tElems;
  hL = PermutationList[#, n] & /@ hElems;
  cover[ts_] := Length@DeleteDuplicates@
     Flatten[Outer[#1[[#2]] &, ts, hL, 1], 1];
  Which[
   cover[tL] === n!, tElems,
   cover[InversePermutation /@ tL] === n!, InversePermutation /@ tElems,
   True, $Failed]];

(* Collision positions from Matchlist verbose output; rescan fallback. *)
CollisionPairs[scan_, h1_, h2_] := Module[{p},
  p = Cases[scan, {_Integer, {p1_Integer, p2_Integer}} :> {p1, p2}, Infinity];
  If[p =!= {}, p,
   Join @@ Table[If[h1[[i]] === h2[[j]], {{i, j}}, {}],
     {i, Length[h1]}, {j, Length[h2]}]]];

(* Function to compare unitary permutation equivalence of ETFs.
   Returns {True, g} with Phi P_g ~ Psi, or {False, {}}. *)
ComPareETFs[etf1_?MatrixQ, etf2_?MatrixQ, tol_: 10^(-8), verbose_: False] :=
 Module[{n, d, k, l, hElements, transversalElements, tensor1, tensor2,
   keys1, keys2, allhashlist1, allhashlist2, Result, pairs, confirmed,
   g, vPrint},
  vPrint[msg_] := If[verbose, Print[msg]];
  n = Length[etf1];
  d = If[n > 0, Length[etf1[[1]]], 0];
  If[Length[etf2] != n || (n > 0 && Length[etf2[[1]]] != d),
   vPrint["Error: Input ETFs must be n x d matrices with the same dimensions."];
   Return[{False, {}}]];
  If[n == 0, vPrint["Warning: Empty ETFs. Considering them equivalent."];
   Return[{True, {}}]];
  If[d == 0, vPrint["Warning: ETFs with zero-length rows. Triple products will be trivial."]];
  vPrint["Starting ETF equivalence comparison for n = " <> ToString[n] <>
    " and d = " <> ToString[d] <> "."];
  {k, l} = OptimalKLCache[n];
  vPrint["Determined optimal k = " <> ToString[k] <> ", l = " <> ToString[l]];
  vPrint["Computing subgroup H..."];
  hElements = GroupElements[PermutationGroup[hGeneratorsCyclesCache[n, k, l]]];
  vPrint["|H| = " <> ToString[Length[hElements]]];
  vPrint["Computing transversal..."];
  transversalElements = EnsureLeftTransversal[
    GenerateTransversalHCache[n, k, l], hElements, n];
  If[transversalElements === $Failed,
   vPrint["Error: cached transversal does not correspond to H."];
   Return[{False, {}}]];
  vPrint["|T| = " <> ToString[Length[transversalElements]]];

  (* Tensors once; every group element is an index gather. *)
  tensor1 = ComputeHermitianTripleProducts[etf1];
  tensor2 = ComputeHermitianTripleProducts[etf2];

  vPrint["Computing ETF1 keys (via T)..."];
  keys1 = ParallelTable[Flatten@PermuteTensor[tensor1, tElem, n],
    {tElem, transversalElements}];
  vPrint["Computing ETF2 keys (via H)..."];
  keys2 = ParallelTable[Flatten@PermuteTensor[tensor2, hElem, n],
    {hElem, hElements}];

  vPrint["Hashing..."];
  allhashlist1 = HashfunctionH[#, tol] & /@ keys1;
  allhashlist2 = HashfunctionH[#, tol] & /@ keys2;

  vPrint["Comparing hashes..."];
  Result = Matchlist[allhashlist1, allhashlist2, True];
  If[Result === False, vPrint["No equivalence found."]; Return[{False, {}}]];

  (* Confirm exactly: a hash match is evidence, not proof. *)
  pairs = CollisionPairs[Result, allhashlist1, allhashlist2];
  vPrint[ToString[Length[pairs]] <> " hash collision(s); confirming..."];
  confirmed = SelectFirst[pairs,
    Norm[keys1[[#[[1]]]] - keys2[[#[[2]]]], Infinity] < tol &, None];
  If[confirmed === None,
   vPrint["All collisions spurious. Not equivalent."]; Return[{False, {}}]];

  (* Isomorphism g = t h^-1; both composition orders tried, verified exactly. *)
  g = With[{t = transversalElements[[confirmed[[1]]]],
     h = hElements[[confirmed[[2]]]]},
    SelectFirst[
     {PermutationProduct[t, InversePermutation[h]],
      PermutationProduct[InversePermutation[h], t]},
     Norm[Flatten[PermuteTensor[tensor1, #, n] - tensor2], Infinity] < tol &,
     None]];
  If[g === None,
   vPrint["Collision confirmed but isomorphism assembly failed."];
   Return[{False, {}}]];

  vPrint["The two ETFs are equivalent. Isomorphism g = " <> ToString[g]];
  {True, g}]


(*Compute full triple-product tensor*)
T=ComputeHermitianTripleProducts[phi,precision];
(*Sum of m-th powers over all i,j,k*)
Total[Flatten[T]^m]]

(*Momemt function of the m-poerws of the triple products*)


DistinctTP[etf1_,tol_:10^(-20)]:=Module[{vals,d=Length[etf1[[1]]],n=Length[etf1]},
vals=Flatten[ComputeHermitianTripleProducts[etf1,150]];DeleteDuplicates[vals,(Abs[Re[#1]-Re[#2]]<tol&&Abs[Im[#1]-Im[#2]]<tol)&]]
(*Computes all the distinct triple products and returns the list*)
DistinctTPL[etf1_,tol_:10^(-20)]:=Module[{vals},
vals=Flatten[ComputeHermitianTripleProducts[etf1,70]];Length[DeleteDuplicates[vals,(Abs[Re[#1]-Re[#2]]<tol&&Abs[Im[#1]-Im[#2]]<tol)&]]]
(*Counts the number of distinct ETFs you have*)
(*You might as well use Length of the result from DitinctTP function to count the number of distinct ETFs*)
