#!bin/bash
set -x

cat > rr.s << EOF
\$keys in
r \$in
rmean
rtrend
w over
q
EOF

for i in *SAC;do
 sac rr.s in ${i}
done


