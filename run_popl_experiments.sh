#!/bin/bash

iter=6
echo "NOTE: Running each iteration for ${iter} iterations."
echo "Set it to 13 to replicate plots from paper. (Slower)"
echo "Press any key to continue..."
read tmp
echo "Running experiment for.."
echo "Fig 1(b). 4cycle topology"
./Frenetic_ProbNetKAT_TE.native 4cycle -ecmp -spf -out 4cycle_intro -iter ${iter}

echo "Fig 1(c). 4cycle topology with link failure"
./Frenetic_ProbNetKAT_TE.native 4cycle -ecmp -spf -break-a 1 -break-b 2 -out 4cycle_intro_break_1_2 -iter ${iter}

echo "Fig 5(c,d,g). Abilene topology"
./Frenetic_ProbNetKAT_TE.native abilene -ecmp -ksp -multi -raeke -out abilene -iter ${iter}

echo "Fig 5(e,f). Abilene topology with faulty links"
./Frenetic_ProbNetKAT_TE.native abilene -lfp 0.1 -ecmp -ksp -multi -raeke -out abilene_fail_0.1 -iter ${iter}

echo "Fig 5(h). Random walk on 4-cycle topology"
./Frenetic_ProbNetKAT_TE.native 4cycle -ecmp -rw -out 4cycle_loop -iter ${iter}
