(* ::Package:: *)

Ith=10^-13;


FitnessQuality[Measured_,Ideal_]:= Module[{max1,min1,max0,min0},
	{min1,max1}=MinMax@Pick[Measured,Ideal,1];
	{min0,max0}=MinMax@Pick[Measured,Ideal,0];
	(*add threshold and sanity check min0>-1*)
	If[min1>max0 && min0>-1,
		(min1-max0)/(max1-min0+Abs@min0+Ith),
		0.]
	]


perms=Permutations[{1,1,0,0,0,0,0}];
configs[d_,p_]:=Table[ 
	Total[IntegerDigits[p,2,5]*2^(7-Flatten@Position[\!\(\*SubscriptBox[\(perms\), \(\(\[LeftDoubleBracket]\)\(d\)\(\[RightDoubleBracket]\)\)]\),0])]+
	Total[inp*2^(7-Flatten@Position[\!\(\*SubscriptBox[\(perms\), \(\(\[LeftDoubleBracket]\)\(d\)\(\[RightDoubleBracket]\)\)]\),1])],
	{ inp,{{0,0},{1,0},{0,1},{1,1}} }];
dps=Catenate@Table[configs[dd,pp],
	{pp,0,31},{dd,21}];
IdealOuts=IntegerDigits[ Range[14],2,4];

IQ128[n_] := Module[{len, dec, perms},
  (
   len = 7; dec = IntegerDigits[n, 2, 128];
   cnts = ConstantArray[0., 16];
   Do[
    	MeasuredOut = Table[dec[[c + 1]], {c, dp}];
    		cnts[[ 1 + FromDigits[MeasuredOut, 2 ]]] += 1. , {dp, dps}];
   GeometricMean@cnts
   )]

FunctionalDiversity[decisions_]:=
Table[
	Max@Table[
	MeasuredOut=Table[decisions[[c+1]],{c,dp}];
		FitnessQuality[MeasuredOut,IdealOut] ,{dp,dps}]
	,{IdealOut,IdealOuts}]


dat=Import["C:\\Users\\Lawrence\\Downloads\\1530373443spider_IQMeasure_dat.csv"];


dat//Dimensions


Table[
decisions=dat[[1+dev*128;;128+dev*128,6]];
Echo@FunctionalDiversity[decisions],{dev,0,69}]


(* ::Input:: *)
(*Do[*)
(**)
(*AlphaSIMOuts=AlphaOuts=Alphascores = AlphanwC= fscores = Table[0,{14}];*)
(**)
(*Do[*)
(*decisions=dat[[1+(dev+10*Np)*128;;128+(dev+10*Np)*128,6]];*)
(*Do[*)
(*MeasuredOut=Table[decisions[[c+1]],{c,dp}];*)
(*SIMOut=Table[dat[[c+1+(dev+10*Np)*128]],{c,dp}];*)
(*Do[*)
(*fscores[[i]]=FitnessQuality[MeasuredOut,IdealOuts[[i]]] ;*)
(* If[fscores[[i]]>Alphascores[[i]],Alphascores[[i]]=fscores[[i]];AlphaOuts[[i]] = MeasuredOut;AlphanwC[[i]]=dp;AlphaSIMOuts[[i]]=SIMOut];*)
(*,{i,14}],{dp,dps}];,{dev,0,9}];*)
(**)
(*Echo@TableForm[Transpose[{{"AND", "0010", "Q", "0100", "P", "XOR", "OR", "NOR", "XNOR", "!P", "1011", "!Q", "1101", "NAND"}, AlphanwC, Alphascores, AlphaOuts,AlphaSIMOuts}]];,*)
(*{Np,0,6}]*)
(**)


(* ::Input:: *)
(*TableForm[Transpose[{{"AND", "0010", "Q", "0100", "P", "XOR", "OR", "NOR", "XNOR", "!P", "1011", "!Q", "1101", "NAND"}, AlphanwC, Alphascores, AlphaOuts,AlphaSIMOuts}]]*)
(**)


dat[[{10,14,11,15}+1+13*128]]
