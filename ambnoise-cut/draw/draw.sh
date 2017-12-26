#!bin/bash
set -x

file="K030.K051.2007.D.SAC K030.K051.3-.SAC K030.K051.3+.SAC"
dist=228.14

TC=40
alpha=5.0
bsl=0.716
bsr=1.546
rs=2.690
bs=3.169


cat > draw.s << EOF
\$keys in
r \$in
color color increment list black red blue
gtext software font 1 size small
plabel 'SNR: Black=${bsl} ${bsr}' position .12 .82 size small
plabel 'Red=${rs}' position .16 .77 size small
plabel 'Blue=${bs}' position .16 .72 size small
title 'DIST=${dist}KM alpha=${alpha} T=${TC}s' size medium
xlabel 'Time/s' size medium
ylabel 'Normalized Waveform' size medium
xlim -150 150
ylim -1 1
p2
EOF
sac draw.s in ${file}
