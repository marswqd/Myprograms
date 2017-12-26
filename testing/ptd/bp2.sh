#!/bin/sh
set -x

cat > bp.sm << EOF
\$keys in out f1 f2 f3 f4
\$default f1 1
\$default f2 2
\$default f3 8
\$default f4 10
r \$in
taper
transfer from none to none freq \$f1 \$f2 \$f3 \$f4 
w \$out
r \$out
p
EOF

sac bp.sm in $1 out $2
rm bp.sm

# 10~60: 0.015 0.016 0.1 0.11
# 10~50: 0.019 0.02  0.1 0.11
# 10~40: 0.024 0.025 0.1 0.11
# 10~30: 0.032 0.033 0.1 0.11
# 10~20: 0.049 0.05  0.1 0.11
 # 5~20: 0.049 0.05  0.2 0.21
# 20~30: 0.032 0.033 0.05 0.051
# 30~40: 0.024 0.025 0.033 0.034
# 10-15~30-40: 0.025 0.033 0.066 0.1 
 
 
 
 
 
 