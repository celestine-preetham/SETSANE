(* ::Package:: *)

(* ::Subsubsection:: *)
(*GA search*)


(* sizeG and fitnessEval[genome_] are externally defined. 
Non-parallelized fitness evaluation *)

Breed[dad_,mom_]:=
(
patrilineage =  RandomInteger[2^(sizeG)];
matrilineage = 2^(sizeG)-patrilineage;
child = BitAnd[patrilineage,dad]+BitAnd[matrilineage,mom];
mutation = FromDigits[RandomChoice[{Abs[#]/sizeG,1}->{1,0}]&/@geneflip,2]; (* mutate to maintain genetic diversity *)
Xchild = BitXor[child,mutation]) 


population=32; Epopulation=2;selectBreeds := Join[Rchildren,Mchildren, Echildren];epochs=1000; 


epoch=0;
generation= RandomInteger[2^sizeG,population];
fitGen =fitnessEval/@generation;
genePool = IntegerDigits[generation,2,sizeG];
geneflip = Sin[ Pi*(Total[genePool]-population/2)/population/2.];


file=OpenAppend[ToString@UnixTime[]<>"GA"<>"dat.csv"];


Do[
genePool = IntegerDigits[generation,2,sizeG];
geneflip = Sin[ Pi*(Total[genePool]-population/2)/population/2.];

(* Random breeding *)
dadRH = RandomInteger[{1,population},population]; momRH = RandomInteger[{1,population},population];
couples = {generation[[dadRH]],generation[[momRH]]}\[Transpose];
Rchildren =Breed@@@couples;
(* Elitist breeding  *)
dadEH = RandomInteger[{1,Epopulation},population]; momEH = RandomInteger[{1,Epopulation},population];
couples = {generation[[dadEH]],generation[[momEH]]}\[Transpose];
Echildren =Breed@@@couples;
(* mutant breeding *)
mutantH = RandomInteger[{1,Epopulation},population];
mutations = 2^RandomInteger[sizeG-1,population]; (* single gene mutants*)
Mchildren = BitXor[generation[[mutantH]],mutations];

children = Join[Rchildren, Echildren , Mchildren] (* selectBreeds *);
breedings = {{dadRH,momRH}\[Transpose],{dadEH,momEH}\[Transpose],mutantH};

(* generation[epoch] = DeleteDuplicates@(generation[epoch]~Join~children);*)
(*generation = (generation~Join~children);

(*fitGen[epoch] = fitGen[epoch]~Join~Parallelize[(fitnessEval/@children)];*)
fitGen = (fitnessEval/@generation);*)

generation = DeleteDuplicates@(generation~Join~children);
fitGen = (fitnessEval/@generation);
survivors = Ordering[-fitGen,population];

generation=generation[[survivors]];
fitGen=fitGen[[survivors]];

epoch=epoch+1;

Export[file,{Flatten@{First@fitGen,generation}}];
Echo@{epoch,UnixTime[],First@fitGen};
,{epochs}]//AbsoluteTiming
