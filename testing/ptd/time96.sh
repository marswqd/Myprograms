#This program predicts the first arrival times of the P, SV and SH arrivals for a given
#model96 file[s]. This program is designed to used in the manner of udfdd in that out-
#put can be placed in a SHELL variable.
#理论震相走时计算(CPS330)

#!/bin/sh
set -x

DIST=195.28
DEPTH=7.9
MOD=tak135sph.mod

time96 -DIST $DIST -EVDP $DEPTH -SV -M $MOD
