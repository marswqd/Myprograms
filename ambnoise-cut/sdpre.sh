#!bin/bash
set -x

for i in *SAC;do
sac << EOF
 r $i
 taper
 transfer from polezero s pzfile to none freq 0.016 0.02 0.2 0.25
*transfer from evalresp to vel freq 0.016 0.02 0.2 0.25
 rmean
 rtr
*taper
*bp c 0.05 0.2 n 4 p 2
*decimate 5
*decimate 5
*decimate 2
 w over
 q
EOF
done

