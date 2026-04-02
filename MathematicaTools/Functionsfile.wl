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


PermuteRows[etf_?MatrixQ,sigma_]:=Module[{n=Length[etf],permList},(*Ensure sigma is always converted to a numerical permutation list.This handles both Cycles objects (like Cycles[{}]) and existing lists consistently.*)permList=PermutationList[sigma,n];
etf[[InversePermutation[permList]]]]

HermitianInnerProduct[v1_?VectorQ,v2_?VectorQ]:=Conjugate[v1] . v2;

ComputeHermitianTripleProducts[etf_?MatrixQ, precision_:40]:=Module[{n=Length[etf]},If[n==0,Return[{}]];
If[Length[etf[[1]]]==0,Return[{N[0,40]}]];
Table[N[HermitianInnerProduct[etf[[i]],etf[[j]]]*HermitianInnerProduct[etf[[j]],etf[[k]]]*HermitianInnerProduct[etf[[k]],etf[[i]]],precision],{i,n},{j,n},{k,n}]]

(*Get you the optimal k and l value for the Small step-big step algorithm*)
FindOptimalKL[n_Integer]:=Module[{sqrtNFact,bestK=0,bestL=0,minDiff=Infinity,kMax,localHSize,currentDiff,k,l},sqrtNFact=Sqrt[N[Factorial[n]]];
bestK=0;
bestL=0;
minDiff=Infinity;
If[n<=1,Return[{0,0}]];
(*kMax limits computation,as k! grows very fast.*)kMax=Min[n,{n-2}];(*Adjusted kMax for improved stability*)While[kMax>0&&Factorial[kMax]>2*sqrtNFact&&kMax>1,kMax--];
If[kMax==0,kMax=1];
For[k=0,k<=kMax,k++,For[l=0,l<=n-k,l++,If[k==0&&l==0,Continue[]];
localHSize=If[l==0||l==1,N[Factorial[k]],N[l*Factorial[k]]];
If[localHSize==0,Continue[]];
currentDiff=Abs[localHSize-sqrtNFact];
If[currentDiff<minDiff,minDiff=currentDiff;
bestK=k;
bestL=l;]]];
{bestK,bestL}]

GenerateSubgroupH[n_,k_,l_]:=Block[{harvested},If[k+l>n||(k+l)==0,Return[{}]];
harvested=Reap[Which[(*Case 1:k=0,only an l-cycle*)k==0,If[l>=2,Sow[Cycles[{Range[1,l]}]]],(*Case 2:k=1,only l-cycle on {2,...,l+1}*)k==1,If[l>=2,Sow[Cycles[{Range[2,l+1]}]]],(*Case 3:k>=2*)k>=2,(Sow[Cycles[{{1,2}}]];(*transposition*)Sow[Cycles[{Range[1,k]}]];(*k-cycle*)If[l>=2,Sow[Cycles[{Range[k+1,k+l]}]]])]][[2,1]];
harvested]

GenerateTransversalH[n_,k_,l_]:=Block[{A,B,C,permsB,permsC,harvested,interleaveCompiled},If[k+l>n||(k+l)==0,Return[{}]];
(*Force numeric lists early*)A=Range[1,k];
B=Range[k+1,k+l];
C=Range[k+l+1,n];
(*Step 2:permutations of B that start with k+1*)permsB=If[Length[B]<=1,{B},Select[Permutations[B],First[#]==First[B]&]];
(*Step 3:permutations of C (may be empty)*)permsC=If[Length[C]==0,{{}},Permutations[C]];
(*Compiled helper now takes pos explicitly*)interleaveCompiled=Compile[{{a,_Integer,1},{b,_Integer,1},{c,_Integer,1},{pos,_Integer,1}},Block[{lenA=Length[a],lenB=Length[b],lenC=Length[c],total,slots,ia=1,ib=1,ic=1},total=lenA+lenB+lenC;
slots=ConstantArray[0,total];
Do[Which[pos[[i]]==0,slots[[i]]=a[[ia]];ia++,pos[[i]]==1,slots[[i]]=b[[ib]];ib++,pos[[i]]==2,slots[[i]]=c[[ic]];ic++],{i,1,total}];
slots],CompilationTarget->"WVM"];
(*Enumerate combinations and use Sow/Reap outside Compile*)harvested=Reap[Do[Do[Module[{lenA=Length[A],lenB=Length[b],lenC=Length[c],total,tuples},total=lenA+lenB+lenC;
tuples=Select[Tuples[{0,1,2},total],Count[#,0]==lenA&&Count[#,1]==lenB&&Count[#,2]==lenC&];
Scan[Sow[interleaveCompiled[A,b,c,#]]&,tuples];],{c,permsC}],{b,permsB}]][[2,1]];
(*Return permutations as Cycles[] form*)Map[PermutationCycles,harvested]];

(*large prime modulus*)
(*Encode complex numbers with tolerance 10^-20*)
EncodeComplex[z_]:=Module[{scale=10^20},
Round[Re[z]*scale]*37+Round[Im[z]*scale]];

(*Order-invariant hash:sum of encodings modulo prime*)
HashfunctionH[Tri_]:=Module[{prime=104729,enc=EncodeComplex/@Flatten[Tri]},
Mod[Total[enc],prime]];

Matchlist[list1_,list2_,verbose_:False]:=Module[{tagged1,tagged2,sorted1,sorted2,i=1,j=1,matches={}},(*Tag each element with its original position*)tagged1=MapIndexed[{#1,First[#2]}&,list1];
tagged2=MapIndexed[{#1,First[#2]}&,list2];
(*Sort both lists by value*)sorted1=SortBy[tagged1,First];
sorted2=SortBy[tagged2,First];
(*Two-pointer scan to find collisions*)While[i<=Length[sorted1]&&j<=Length[sorted2],With[{val1=First[sorted1[[i]]],pos1=Last[sorted1[[i]]],val2=First[sorted2[[j]]],pos2=Last[sorted2[[j]]]},Which[val1==val2,AppendTo[matches,{val1,{pos1,pos2}}];
i++;j++,val1<val2,i++,True,j++]]];
(*Return True/False and optional verbose output*)If[matches==={},False,If[verbose,Column[{True,"Collisions found:",TableForm[matches,TableHeadings->{None,{"value","{pos in list1, pos in list2}"}}]}],True]]]

OptimalKLCache[n_]:=OptimalKLCache[n]=FindOptimalKL[n];
hGeneratorsCyclesCache[n_,k_,l_]:=hGeneratorsCyclesCache[n,k,l]=GenerateSubgroupH[n,k,l];

GenerateTransversalHCache[n_,k_,l_]:=GenerateTransversalHCache[n,k,l]=GenerateTransversalH[n,k,l];

(*Function to Compare unitary permutation equivalence of etfs*)
ComPareETFs[etf1_?MatrixQ,etf2_?MatrixQ,tol_:10^(-20),verbose_:False]:=Module[{n,d,k,l,hGeneratorsCycles,hElements,transversalElements,etf1TransformedHashes,etf2TransformedHashes,allhashlist1,allhashlist2,Result,vPrint},vPrint[msg_]:=If[verbose,Print[msg]];
n=Length[etf1];
d=If[n>0,Length[etf1[[1]]],0];
If[Length[etf2]!=n||(n>0&&Length[etf2[[1]]]!=d),vPrint["Error: Input ETFs must be n x d matrices with the same dimensions."];
Return[{False,{}}]];
If[n==0,vPrint["Warning: Empty ETFs. Considering them equivalent."];
Return[{True,{}}]];
If[d==0,vPrint["Warning: ETFs with zero-length rows. Triple products will be trivial."]];
vPrint["Starting ETF equivalence comparison for n = "<>ToString[n]<>" and d = "<>ToString[d]<>"."];
{k,l}=OptimalKLCache[n];
vPrint["Determined optimal k = "<>ToString[k]<>", l = "<>ToString[l]];
vPrint["Computing subgroup H..."];
hGeneratorsCycles=hGeneratorsCyclesCache[n,k,l];
hElements=GroupElements[PermutationGroup[hGeneratorsCycles]];
vPrint["|H| = "<>ToString[Length[hElements]]];
vPrint["Computing S_"<>ToString[n]<>" and transversal..."];
transversalElements=GenerateTransversalHCache[n,k,l];
vPrint["|T| = "<>ToString[Length[transversalElements]]];
vPrint["Computing ETF1 hashes (via H)..."];
etf1TransformedHashes=ParallelTable[ComputeHermitianTripleProducts[PermuteRows[etf1,hElem]],
{hElem,hElements}];
vPrint["ETF1 hash count: "<>ToString[Length[etf1TransformedHashes]]];
vPrint["Computing ETF2 hashes (via T)..."];
etf2TransformedHashes=ParallelTable[ComputeHermitianTripleProducts[PermuteRows[etf2,tElem]],{tElem,transversalElements}];
vPrint["ETF2 hash count: "<>ToString[Length[etf2TransformedHashes]]];
vPrint["Flattening hashes..."];
allhashlist1=HashfunctionH/@etf1TransformedHashes;
allhashlist2=HashfunctionH/@etf2TransformedHashes;
vPrint["Comparing all possible pairs of hashes..."];
Result=Matchlist[allhashlist1,allhashlist2];
If[Result,vPrint["The two ETFs are equivalent."];True,vPrint["No equivalence found."];False]]


MoM[phi_?MatrixQ,m_Integer?Positive,precision_:70]:=Module[{T},
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
