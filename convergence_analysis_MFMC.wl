(* ::Package:: *)

(* MF *)
vals=Table[
takeStep[];
Flatten@{n,IO,IB,dI},{10}];
Dynamic@(ListLinePlot/@Transpose@vals)


Do[
takeStep[];
vals=Append[Drop[vals,1],Flatten@{n,IO,IB,dI}];Pause[0.1],{1000}]


(* MC *)
valsMC=Table[
RandomJump[];
Flatten@{n,nO,nB},{10}];
Dynamic@(ListStepPlot/@Transpose@valsMC)


Do[
RandomJump[];
valsMC=Append[Drop[valsMC,1],Flatten@{n,nO,nB}];Pause[0.1],{1000}]
