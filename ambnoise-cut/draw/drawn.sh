#!bin/bash
set -x

file="K030.K051.2007.D.SAC K030.K051.3-.SAC K030.K051.3+.SAC"
dist=228.14

TC=30
alpha=100.0


cat > draw.s << EOF
\$keys in
r \$in
color color increment list black red blue
gtext software font 1 size small
title 'DIST=${dist}KM alpha=${alpha} T=${TC}s' size medium
xlabel 'Time/s' size medium
ylabel 'Normalized Waveform' size medium
xlim -200 0
ylim -1 1
p2
EOF
sac draw.s in ${file}
