#!bin/bash
#set -x

pgm=snr
sac=(`ls P.*.Z.SAC`);nsac=${#sac[@]}

# sacZ=(`ls P.*.Z.SAC`);lsacZ=${#sacZ[@]}
# sacR=(`ls P.*.R.SAC`);lsacR=${#sacR[@]}
# sacT=(`ls P.*.T.SAC`);lsacT=${#sacT[@]}
# echo "Z files:" $lsacZ
# echo "R files:" $lsacR 
# echo "T files:" $lsacT
# read -n 1 -p "Press any key to continue..." 
# echo ----------------------------

cat > ${pgm}.in << EOF
${nsac}
EOF
for ((m=0;m<nsac;m=m+1));do
 echo ${sac[m]} >> ${pgm}.in
done

gfortran ${pgm}.f90 -o ${pgm}
./${pgm}






