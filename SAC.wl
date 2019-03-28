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
