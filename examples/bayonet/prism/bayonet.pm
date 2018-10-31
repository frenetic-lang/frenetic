dtmc

const int k;

module Bayonet

  // local state
  sw : [0..4*k-1] init 0;
  pt : [-1..2] init 0;
  at_sw : [0..1] init 1;
  delivered : [0..1] init 0;
  
  [] at_sw=1 & mod(sw, 4)=0 -> 0.5 : (pt'=1) & (at_sw'=0) + 0.5 : (pt'=2) & (at_sw'=0);
  [] at_sw=1 & mod(sw, 4)=1 -> 1 : (pt'=2) & (at_sw'=0);
  [] at_sw=1 & mod(sw, 4)=2 -> 0.999 : (pt'=1) & (at_sw'=0) + 0.001 : (pt'=-1) & (at_sw'=0);
  [] at_sw=1 & mod(sw, 4)=3 -> 1 : (pt'=1) & (at_sw'=0);

  [] at_sw=0 & pt>=0 & sw+pt<4*k -> 1 : (sw'=sw+pt) & (pt'=mod(sw, 3)) & (at_sw'=1);
  [] at_sw=0 & sw=4*k-1 -> 1 : (delivered'=1);
  
endmodule
