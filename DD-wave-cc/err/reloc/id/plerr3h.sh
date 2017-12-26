#bin/sh
#set -x

data=err3h.dat
J=X4i/3i
R=0/20/0/1000
ps=err3h.ps

awk '{print $1,$2}' ${data} | psxy -J${J} -R${R} \
-Ba4f2:error/km:/200:NUM.:WnSe -W0.01i,black -Sc0.1i -K > ${ps}
awk '{print $1,$2}' ${data} | psxy -J -R -W0.01i,black -K -O >> ${ps}

awk '{print $1,$3}' ${data} | psxy -J -R -W0.01i,black -Ss0.1i -K -O >> ${ps}
awk '{print $1,$3}' ${data} | psxy -J -R -W0.01i,black -K -O >> ${ps}

awk '{print $1,$4}' ${data} | psxy -J -R -W0.01i,black -St0.1i -K -O >> ${ps}
awk '{print $1,$4}' ${data} | psxy -J -R -W0.01i,black -K -O >> ${ps}

# psxy -J -R -W0.01i,black -Sc0.1i -K -O << ! >> ${ps}
# 0.19 235
# !
# psxy -J -R -W0.01i,black -K -O << ! >> ${ps}
# 0.18 235
# 0.2 235
# !
# psxy -J -R -W0.01i,black -Ss0.1i -K -O << ! >> ${ps}
# 0.19 215
# !
# psxy -J -R -W0.01i,black -K -O << ! >> ${ps}
# 0.18 215
# 0.2 215
# !
# psxy -J -R -W0.01i,black -St0.1i -K -O << ! >> ${ps}
# 0.19 195
# !
# psxy -J -R -W0.01i,black -K -O << ! >> ${ps}
# 0.18 195
# 0.2 195
# !
# pstext -J -R -Gblack -K -O << ! >> ${ps}
# 0.225 235 15 0 35 CM 南北向
# 0.225 215 15 0 35 CM 东西向
# 0.225 195 15 0 35 CM 深度
# !
# psxy -J -R -W0.01i,black -K -O << ! >> ${ps}
# 0.174 250
# 0.174 182
# 0.25 182
# !



#gmtset ANNOT_FONT_PRIMARY STHeiti-Regular--GB-EUC-H
# pslegend -Dx3i/0i/1i/1i/BL -J -R -Gwhite -F -K -O << ! >> ${ps}
# S 0.3i c 0.1i - 0.01i,black 0.5i nanbei
# S 0.3i s 0.1i - 0.01i,black 0.5i dongxi
# S 0.3i - 0.1i - 0.01i,black 0.5i sendu
# !

