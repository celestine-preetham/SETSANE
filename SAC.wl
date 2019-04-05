(*Strict avalanche criterion*)
SAC[code_, n_] := Module[{decision = IntegerDigits[code, 2, 2^n]},
  Table[
   (*Partition into blocks of 2^(i-1) *)
   
   p = Partition[decision, 2^(i - 1)];
   oddP = Flatten@p[[1 ;; -1 ;; 2]];
   evenP = Flatten@p[[2 ;; -1 ;; 2]];
   1.*Mean@Thread[BitXor[oddP, evenP]]
   , {i, n}]
  ]

(*Symmetry test for grid. Expectation for random integer 2^128 is 0.43 but can be 0 for physical devices*)
SymmetryTest[code_] := Module[{decision = IntegerDigits[code, 2, 128]},
  1.0*Mean@
    Table[{e7, e6, e5, e4, e3, e2, e1} = IntegerDigits[i - 1, 2, 7];
     Mi = 1 + FromDigits[{e7, e1, e2, e3, e4, e5, e6}, 2];
     BitXor[ decision[[i]], decision[[Mi]]], {i, 128}]]

(*for 7+1 electrode system*)
Inhibition[code_] := Module[{decision = IntegerDigits[code, 2, 2^7]},
  Table[
    (*Partition into blocks of 2^(i-1) *)
      p = Partition[decision, 2^(i - 1)];
      oddP = Flatten@p[[1 ;; -1 ;; 2]]; (*index i is 1*)
      evenP = Flatten@p[[2 ;; -1 ;; 2]];(*index i is 0*)
      1.*Mean@Thread[evenP*(1 -oddP)] (*find mean number of cases with 1 for evenP and 0 for oddP *)
        ,{i, 7}]
  ]
