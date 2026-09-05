(* exactify_validate.wl — called by the etf binary via wolframscript.
   Usage: wolframscript -file exactify_validate.wl <in.gos> <out.gos> <d> <n>
   Reads a numeric frame, exactifies its triple products, VALIDATES it as an ETF,
   and writes the frame back numeric (high precision) for C++ to consume.
   Exits nonzero if validation fails, so C++ knows not to export.
   Depends on convertFrameData.wl, exactification.wl, frameInvariants.wl (same dir). *)

dir = DirectoryName[$InputFileName];
Get[FileNameJoin[{dir, "convertFrameData.wl"}]];
Get[FileNameJoin[{dir, "exactification.wl"}]];
Get[FileNameJoin[{dir, "frameInvariants.wl"}]];

args = Rest @ $ScriptCommandLine;
If[Length[args] < 4,
  Print["usage: exactify_validate.wl <in.gos> <out.gos> <d> <n>"]; Exit[1]];
inPath = args[[1]]; outPath = args[[2]];
d = ToExpression @ args[[3]]; n = ToExpression @ args[[4]];

gos = Flatten @ Import[inPath, "Table"];
Phi = SOfromGoS[N[gos, 30], {d, n}];

(* build Gram, exactify its triple products *)
G  = GMfromSO[Phi];
TP = TPfromGM[G];
TPexact = exactifyTP[TP];        (* exact algebraic triple products (kept in MMA) *)

(* validate as ETF *)
welch = Sqrt[(n - d)/(d (n - 1))];
off   = Select[Flatten[Abs[G - IdentityMatrix[n]]], # > 10^-6 &];
cohOK = AllTrue[off, Abs[# - welch] < 10^-6 &];
projOK = Max[Abs[G . G - (n/d) G]] < 10^-6;
If[! (cohOK && projOK),
  Print["validation FAILED: cohOK=", cohOK, " projOK=", projOK]; Exit[2]];

(* write frame back NUMERIC high-precision for C++ *)
Export[outPath, N[GoSfromSO[Phi], 30], "Table"];
Print["exactify_validate.wl: validated (d=", d, " n=", n, "), wrote ", outPath];
