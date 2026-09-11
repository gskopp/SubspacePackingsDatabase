(* ::Package:: *)

(* Created by Gene Kopp, Aug 2025; updated Mar 2026 *)
(* Functions to convert between vector list, Gram matrix, and triple products *)


(* TYPES *)
(* SO = sythesis operator = "frame" = "short, fat matrix"
   = matrix whose columns are frame vectors *)
(* all columns of SO must be unit vectors *)
(* AO = analysis operator = conjugate transpose of syntesis operator *)
(* all rows of AO must be unit vectors *)
(* GM = Gram matrix *)
(* TP = triple product tensor *)

(* CONVERSION FUNCTIONS *)
(* Normalized (unit) analysis operator from unnormalized analysis operator *)
normalizeAO[V_] := Normalize /@ V

(* Normalized (unit) synthesis operator from unnormalized synthesis operator *)
normalizeSO[Phi_] := normalizeAO[Phi\[ConjugateTranspose]]\[ConjugateTranspose]

(* Gram matrix from vector list *)
GMfromSO[Phi_] := Phi\[ConjugateTranspose] . Phi

(* Triple product tensor from Gram matrix *)
TPfromGM[G_] :=
  #*Transpose[#, {3, 1, 2}]*Transpose[#, {2, 3, 1}] &@ ConstantArray[G, Length@G]

(* Triple product slice from Gram matrix *)
TPslicefromGM[G_, i_ : 1] := G*Outer[Times, G[[i]], G[[All, i]]]

(* Triple product tensor from vector list *)
TPfromSO[Phi_] := TPfromGM[GMfromSO[Phi]]

(* Vector list from Gram matrix *)
Options[SOfromGM] = {Tolerance -> 10^(-6)};
SOfromGM[G_, r_ : Automatic, OptionsPattern[]] :=
 Module[{d, U, \[CapitalLambda], V},
  d = r;
  If[d === Automatic,
  d = MatrixRank[G, Tolerance -> OptionValue[Tolerance]]];
   {U, \[CapitalLambda], V} = SingularValueDecomposition[G, UpTo[d]];
   (U . Sqrt[\[CapitalLambda]])\[ConjugateTranspose]
  ] /; IntegerQ[r] || r === Automatic
ResourceFunction["AddCodeCompletion"]["SOfromGM"][
  None, RepeatOptions[SOfromGM, 1]];

(* Gram matrix from triple product slice *)
GMfromTPslice[TPS_, i_ : Automatic] := Module[{j, const},
  If[i === Automatic,
   j = PositionLargest[Abs@Diagonal[TPS]][[1]],
   j = i];
  const = ConstantArray[TPS[[j]], Length@TPS];
  TPS/Sqrt[const*Transpose[const]]
  ]

(* Gram matrix from triple product tensor *)
(* currently implemented when SO has a vector not orthogonal to any other vector *)
Options[GMfromTP] = {Tolerance -> 10^(-10)};
GMfromTP[TP_, OptionsPattern[]] := Module[{n, j},
  n = Length[TP];
  j = Select[Range@Length[TP],
     ! AnyTrue[TP[[#]], Abs[##] < OptionValue[Tolerance] &, 2] &, 1];
  If[j === {}, Return[$Failed]];
  j = j[[1]];
  GMfromTPslice[TP[[j]], j]
  ]
ResourceFunction["AddCodeCompletion"]["GMfromTP"][None, RepeatOptions[GMfromTP]];

(* Vector list from triple product tensor *)
(* currently implemented when SO has a vector not orthogonal to any other vector *)
Options[SOfromTP] = Options[SOfromGM];
SOfromTP[TP_, r_Integer : Automatic, opts : OptionsPattern[]] :=
 SOfromGM[GMfromTP[TP], r, opts]
ResourceFunction["AddCodeCompletion"]["SOfromTP"][
  None, RepeatOptions[SOfromTP, 1]];

(* Faithful matrix plot for synthesis operator or Gram matrix *)
FrameVisualize[M_] := MatrixPlot[
    MapThread[Hue, {(Arg[M] + Pi)/(2 Pi), Abs[M]}, 2], Frame -> False]

(* Game of Sloanes representation of a synthesis operator *)
GoSfromSO[Phi_] := Join[Re[#], Im[#]] &@ Flatten[Phi\[Transpose]]

(* Synthesis operator from a Games of Sloanes representation *)
SOfromGoS[gos_, {d_, n_}] :=
 Transpose@ArrayReshape[gos[[;; d n]] + I gos[[d n + 1 ;;]], {n, d}]

(* Convert between triple product tensor and one slice of the triple
   product tensor *)
TPslicefromTP[TP_, i_ : 1] := TP[[i]]
TPfromTPslice[TPS_, i_ : 1] := Module[{T1, T2, T3},
  T1 = ConstantArray[TPS/TPS[[i]], Length@TPS];
  T2 = Transpose[T1, {3, 1, 2}];
  T3 = Transpose[T1, {2, 3, 1}];
  TPS[[i, i]]*T1*T2*T3
  ]

(* Converts an array to a lookup table *)
Options[arraytoLUT] = {WorkingPrecision -> Automatic, "PackArray" -> Automatic};
arraytoLUT[array_, OptionsPattern[]] :=
 Module[{aprec, wprec, pack ,narray, distinct, LUT, dims, positions, base},
  aprec = Precision[array];
  wprec = OptionValue[WorkingPrecision];
  pack = OptionValue["PackArray"];
  Which[
   wprec === pack === Automatic,
   If[aprec >= MachinePrecision + 5,
    wprec = MachinePrecision;
    pack = True,
    wprec = Max[1, aprec - 5];
    pack = False;
    ],
   wprec === Automatic && pack =!= Automatic,
   wprec = If[aprec >= MachinePrecision + 5, MachinePrecision, Max[1, aprec - 5]],
   wprec =!= Automatic && pack === Automatic,
   pack = If[wprec === MachinePrecision, True, False]
   ];
  narray = SetPrecision[array, wprec + 5];
  If[pack === True,
   narray = Developer`ToPackedArray[narray, Complex],
   narray = Chop[narray, 10^(5 - SetPrecision[Accuracy[narray], Infinity])];
   narray = SetPrecision[narray, wprec];
   ];
  dims = Dimensions[narray];
  narray = Flatten[narray];
  distinct = PositionIndex[narray];
  LUT = AssociationThread[Keys[distinct] -> Range[0, Length[distinct] - 1]];
  LUT = ArrayReshape[Lookup[LUT, narray], dims];
  positions = First /@ Values[distinct];
  base = Reverse@FoldList[Times, 1, Reverse@dims[[2 ;;]]];
  positions = Mod[Quotient[# - 1, base], dims] & /@ positions + 1;
  distinct = Extract[array, positions];
  {distinct, LUT}
  ]
ResourceFunction["AddCodeCompletion"]["arraytoLUT"][
  None, RepeatOptions[arraytoLUT]];

(* Converts a lookup table to an array *)
arrayfromLUT[LUT_] := 
 LUT[[2]] /. AssociationThread[Range[0, Length[LUT[[1]]] - 1] -> LUT[[1]]]
