(* ::Package:: *)

(* Created by Gene Kopp, Aug 2025 *)
(* Functions to convert tuples of inexact numbers to exact numbers, assuming they
   are Galois conjugate over a small field *)
(* Includes exactification of triple products *)


(* Exactification using elementary symmetric polynomials and RootApproximant *)
(* Assumes that elements of "tuple" come in complex conjugate pairs (or are real) *)
(* If "RationalCoefficients" is set to True, instead use Rationalize on coefficeints *)
(* "SimplificationMethod" is the function used to simplfy the algebraic numbers
   to be output by exactifyTuple *)
(* If "NumericalRefinement" is set to True, instead of simplifying, approximate
   to higher precision and then apply RootApproximant again *)
(* Increase the precision by a factor of "RefinementFactor" when using
   "NumericalRefinement" *)
Options[exactifyTuple] = {"RationalCoefficients" -> False,
   "SimplificationMethod" -> RootReduce, "NumericalRefinement" -> False,
   "RefinementFactor" -> 10, Parallelize -> False};
exactifyTuple[tuple_, OptionsPattern[]] :=
 Module[{seqPara, deg, ESP, X, f, roots, incr, j, nrprec, SM},
  If[OptionValue[Parallelize],
   SetAttributes[seqPara, HoldAll];
   seqPara[expr_] := Parallelize[expr, Method -> "FinestGrained"],
   seqPara = Identity;
   ];
  deg = Length[tuple];
  ESP = CoefficientList[Expand[Times @@ (X - tuple)], X];
  If[OptionValue["RationalCoefficients"],
   ESP = Rationalize[ESP, 10^(-0.75 Precision[tuple])],
   ESP = seqPara@RootApproximant[ESP];
   ];
  f = ESP . X^Range[0, deg];
  roots = {};
  incr = 1;
  For[j = 1, j <= deg, j+= incr,
   AppendTo[roots, Root[f, j]];
   If[incr == 1 && Im[roots[[-1]]] =!= 0, incr = 2];
   ];
  If[OptionValue["NumericalRefinement"],
   nrprec = OptionValue["RefinementFactor"]*Precision[tuple];
   roots = seqPara[RootApproximant@N[#, nrprec] & /@ roots],
   SM = OptionValue["SimplificationMethod"];
   roots = seqPara[SM[#] & /@ roots];
   ];
  roots = DeleteDuplicates@Join[roots, Conjugate[roots]];
  Flatten@Nearest[roots, tuple, 1]
  ]
ResourceFunction["AddCodeCompletion"]["exactifyTuple"][
  None, RepeatOptions[exactifyTuple]];

(* Exactify a lookup table of triple products, taking advantage of possible
   Galois conjugates *)
Options[exactifyLUT] = Options[exactifyTuple];
exactifyLUT[LUT_, opts : OptionsPattern[]] :=
 {exactifyTuple[LUT[[1]], opts], LUT[[2]]}
ResourceFunction["AddCodeCompletion"]["exactifyLUT"][
  None, RepeatOptions[exactifyLUT]];

(* Exactify a triple product tensor, taking advantage of possible Galois conjugates *)
Options[exactifyTP] = Join[Options[arraytoLUT], Options[exactifyLUT]];
exactifyTP[TP_, opts : OptionsPattern[]] := Module[{fopts, tmp},
  fopts = FilterRules[{opts}, Options[arraytoLUT]];
  tmp = arraytoLUT[TP, fopts];
  fopts = FilterRules[{opts}, Options[exactifyLUT]];
  tmp = exactifyLUT[tmp, fopts];
  arrayfromLUT[tmp]
  ]
ResourceFunction["AddCodeCompletion"]["exactifyTP"][
  None, RepeatOptions[exactifyTP]];

(* Exactify a lookup table of triple products naively using RootApproximant *)
Options[exactifyLUTalt] = {Parallelize -> False};
exactifyLUTalt[LUT_, OptionsPattern[]] := Module[{seqPara, distinct},
  If[OptionValue[Parallelize],
   SetAttributes[seqPara, HoldAll];
   seqPara[expr_] := Parallelize[expr, Method -> "FinestGrained"],
   seqPara = Identity;
   ];
  distinct = Select[LUT[[1]], Im[#] >= 0 &];
  distinct = seqPara@RootApproximant[distinct];
  distinct = DeleteDuplicates@Join[distinct, Conjugate[distinct]];
  distinct = Flatten@Nearest[distinct, LUT[[1]], 1];
  {distinct, LUT[[2]]}
  ]
ResourceFunction["AddCodeCompletion"]["exactifyLUTalt"][
  None, RepeatOptions[exactifyLUTalt]];

(* Exactify a triple product tensor naively using RootApproximant *)
Options[exactifyTPalt] = Join[Options[arraytoLUT], Options[exactifyLUTalt]];
exactifyTPalt[TP_, opts : OptionsPattern[]] := Module[{fopts, tmp},
  fopts = FilterRules[{opts}, Options[arraytoLUT]];
  tmp = arraytoLUT[TP, fopts];
  fopts = FilterRules[{opts}, Options[exactifyLUTalt]];
  tmp = exactifyLUTalt[tmp, fopts];
  arrayfromLUT[tmp]
  ]
ResourceFunction["AddCodeCompletion"]["exactifyTPalt"][
  None, RepeatOptions[exactifyTPalt]];
