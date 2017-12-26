#!bin/bash

mkdir 2009cut
cd 2009

SAC_DISPLAY_COPYRIGHT=0
file=(`ls P.*.*.Z.SAC`);nfile=${#file[@]}
for((i=0;i<nfile;i++));do
 Z=${file[i]}
 R=${Z/.Z./.R.}
 T=${Z/.Z./.T.}
 if [ -f "C$R" -a -f "C$T" ]; then
  continue
 fi
 info=(`saclst a t0 f ${Z}`)
 P=${info[1]}
 S=${info[2]}
 flag=`echo $P $S | awk '{if($1!=-12345.0 && $2!=-12345.0){print "a"}\
                          else if($1!=-12345.0 && $2==-12345.0){print "b"}\
                          else if($1==-12345.0 && $2!=-12345.0){print "c"}\
                          else{print "d"}}'`
 echo $Z $P $S $flag
 #continue
 if [ "$flag" == "a" ];then
sac << EOF
cuterr fillz; cut a -5 t0 5
r $Z $R $T
w C$Z C$R C$T
q
EOF
 fi
 if [ "$flag" == "b" ];then
sac << EOF
cuterr fillz; cut a -5 5
r $Z $R $T
w C$Z C$R C$T
q
EOF
 fi
 if [ "$flag" == "c" ];then
sac << EOF
cuterr fillz; cut t0 -5 5
r $Z $R $T
w C$Z C$R C$T
q
EOF
 fi
done

mv CP.*.*.?.SAC ../2009cut

cd ..




