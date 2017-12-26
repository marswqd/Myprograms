#sac宏文件。功能：挑选波形。将不好的波形文件删除掉，并挑选合格的波形进行震相标注。
#终端中运行 sac chwave.sm

mkdir delete CCSAC CTSAC

sacZ=(`ls P.*.Z.SAC`);nsacZ=${#sacZ[@]}
#sacR=(`ls P.*.R.SAC`);nsacR=${#sacR[@]}
#sacT=(`ls P.*.T.SAC`);nsacT=${#sacT[@]}
#sacN=(`ls P.*.N.SAC`);nsacN=${#sacN[@]}
#sacE=(`ls P.*.E.SAC`);nsacE=${#sacE[@]}
echo "Z files:" $nsacZ
#echo "R files:" $nsacR 
#echo "T files:" $nsacT
#echo "N files:" $nsacN
#echo "E files:" $nsacE 
#read -n 1 -p "Press any key to continue..." 
#echo ----------------------------

for ((i=0;i<nsacZ;i=i+1));do
 Z=${sacZ[i]}
 R=${Z/.Z./.R.}
 T=${Z/.Z./.T.}
 N=${Z/.Z./.N.}
 E=${Z/.Z./.E.}
 
 cat > chw.sm << EOF
 r ${Z} ${R} ${T}
 lh files 1 dist a t0 evdp az mag
 p1
 setbb resp ( reply "press d to delete, s to save, a to ppk, other words to next " )
 if %resp eq "d"
  *sc rm ${Z} ${R} ${T}
  mv ${Z} ${R} ${T} delete/
 elseif %resp eq "s"
  cp ${Z} ${R} ${T} CCSAC/
  mv ${Z} ${R} ${T} CTSAC/  
 elseif %resp eq "a"   
  *apk 
  ppk p 3 m on        
  w over
  cp ${Z} ${R} ${T} CCSAC/
  mv ${Z} ${R} ${T} CTSAC/
 else
  mv ${Z} ${R} ${T} CTSAC/ 
 endif
 q
EOF
sac chw.sm
done
  
  
  
  
  
  
  
