(* ::Package:: *)

(* Created by Gene Kopp, Mar 2026 *)
(* Functions to compute invariants of frames *)


(* List of distict triple products, including degenerate ones *)
(* First argument can be an a frame, a Gram matrix, a triple product tensor,
   a triple product slice, a triple product lookup table, or a triple product
   slice lookup table *)
(* The available option are
   WorkingPrecision: Determines the number of signifcant digits used for
     comparing triple products
   PackArray: Determines whether the triple product tensor is to be packed *)
Options[distinctTP] = {WorkingPrecision -> Automatic, "PackArray" -> Automatic};
distinctTP[array_, OptionsPattern[]] := 
 Module[{type, TP, nTP, aprec, wprec, pack, dims, positions, base},
  type = arrayType[array];
  If[type === "TP LUT", Return@array[[1]]];
  Which[
   type === "TPS LUT", TP = TPfromTPslice@arrayfromLUT[array],
   type === "SO",
   TP = TPfromSO[array],
   type === "GM", TP = TPfromGM[array],
   type === "TPS", TP = TPfromTPslice[array],
   type === "TP", TP = array,
   True, Return[$Failed]
   ];
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
  nTP = SetPrecision[TP, wprec + 5];
  If[pack === True,
   nTP = Developer`ToPackedArray[nTP, Complex],
   nTP = Chop[nTP, 10^(5 - SetPrecision[Accuracy[nTP], Infinity])];
   nTP = SetPrecision[nTP, wprec];
   ];
  dims = Dimensions[nTP];
  nTP = Flatten[nTP];
  positions = First /@ Values@PositionIndex[nTP];
  base = Reverse@FoldList[Times, 1, Reverse@dims[[2 ;;]]];
  positions = Mod[Quotient[# - 1, base], dims] & /@ positions + 1;
  Extract[TP, positions]
  ]
ResourceFunction["AddCodeCompletion"]["distinctTP"][
  None, RepeatOptions[distinctTP]];

(* Number of distict triple products, including degenerate ones *)
Options[numberTP] = Options[distinctTP];
numberTP[array_, opts : OptionsPattern[]] := Length@distinctTP[array, opts]
ResourceFunction["AddCodeCompletion"]["numberTP"][None, RepeatOptions[numberTP]];

(* Core code of moment and momentnd *)
Options[momentCore] = {PrecisionGoal -> Automatic, Method -> Automatic,
   "ND" -> False};
momentCore[array_, m_, OptionsPattern[]] := Module[{gprec, wprec, type, Tm, CS},
  gprec = OptionValue[PrecisionGoal];
  If[gprec === Automatic, gprec = Precision[array]];
  wprec = gprec + If[gprec === MachinePrecision, 0, 5];
  type = arrayType[array];
  Which[
   type === "TP LUT",
   Tm = array;
   Tm[[1]] = N[Tm[[1]], wprec]^m;
   Tm = arrayfromLUT[Tm],
   type === "TPS LUT",
   Tm = array;
   Tm[[1]] = N[Tm[[1]], wprec]^m;
   Tm = TPfromTPslice@arrayfromLUT[Tm],
   type === "SO", Tm = TPfromGM[GMfromSO[N[array, wprec]]^m],
   type === "GM", Tm = TPfromGM[N[array, wprec]^m],
   type === "TPS", Tm = TPfromTPslice[N[array, wprec]^m],
   type === "TP", Tm = N[array, wprec]^m,
   True, Return[$Failed]
   ];
  CS = OptionValue[Method];
  If[CS === Automatic,
   If[wprec === MachinePrecision,
    CS = "CompensatedSummation",
    CS = Automatic
    ]
   ];
  If[OptionValue["ND"],
   Do[Tm[[i, i, All]] = Tm[[i, All, i]] = Tm[[All, i, i]] = 0, {i, Length[Tm]}]
   ];
  N[Total[Tm, 3, Method -> CS], gprec]
  ]
ResourceFunction["AddCodeCompletion"]["momentCore"][
  None, None, RepeatOptions[momentCore]];

(* Moments [sum of powers of triple products] *)
(* Passing the option Method -> "CompensatedSummation" uses "CompensatedSummation"
   method with Total. "CompensatedSummation" is always used if array has precision
   equal to MachinePrecision *)
Options[moment] = {PrecisionGoal -> Automatic, Method -> Automatic};
moment[array_, m_, opts : OptionsPattern[]] :=
 momentCore[array, m, opts, "ND" -> False]
ResourceFunction["AddCodeCompletion"]["moment"][
  None, None, RepeatOptions[moment]];

(* Nondiagonal moments [sum of powers of totally nondiagonal triple products] *)
(* Passing the option Method -> "CompensatedSummation" uses "CompensatedSummation"
   method with Total. "CompensatedSummation" is always used if array has precision
   equal to MachinePrecision *)
Options[momentnd] = Options[moment];
momentnd[array_, m_, opts : OptionsPattern[]] :=
 momentCore[array, m, opts, "ND" -> True]
ResourceFunction["AddCodeCompletion"]["momentnd"][
  None, None, RepeatOptions[momentnd]];

(* Compute a general m-product with index set Indices_ *)
mproductfromGM[G_, Indices_] := Module[{m, wrapIndices},
  m = Length[Indices];
  wrapIndices = Append[Indices, Indices[[1]]];
  Product[G[[wrapIndices[[i]], wrapIndices[[i + 1]]]], {i, 1, m}]
  ]

mproductfromSO[Phi_, Indices_] := mproductfromSO[GMfromSO[Phi], Indices];

TupleFromIndex = ResourceFunction["TupleFromIndex"];

(* Compute the sum of all m-products with index set of shape Indices_ *)
(* For example, the index set {a, b, c, a, b, c} yields the second moment of the
   triple products *)
(* I think these generate all S_n-invariants [i.e., projective permutation
   unitary invariants] *)
Options[generalSnInvariantfromGM] = {"LimitMemory" -> False};
generalSnInvariantfromGM[G_, Indices_, opts : OptionsPattern[]] := Block[{IndexSet, n, k, m, j},
  IndexSet = DeleteDuplicates[Indices];
  n = Length[G];
  k = Length[IndexSet];
  m = 0;
  If[OptionValue["LimitMemory"],
  For[j = 1, j <= n^k, m = m + mproductfromGM[G, Indices /. Thread[IndexSet -> (1 + TupleFromIndex[j,k])]]; j++],
  m = Plus @@ (mproductfromGM[G, #] &
      /@ (Indices /. (Thread[IndexSet -> #] & /@ Tuples[Range[n], k])))
      ];
  m
  ]

Options[generalSnInvariantfromSO] = Options[generalSnInvariantfromGM];
generalSnInvariantfromSO[Phi_, Indices_, opts : OptionsPattern[]] := 
  generalSnInvariantfromGM[GMfromSO[Phi], Indices, opts];
