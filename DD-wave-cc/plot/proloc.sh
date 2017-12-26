# This script plots up a cross-section of seismicity data and an epicenter map
# data file is hypoDD's output file hypoDD.loc
#!/bin/sh

#Set parameters for plot
psfile=proloc.ps
locfile=hypoDD.loc
R=103.8/104.2/27.4/27.75
B=f5ma10m
J=M5i
#Set parameters for cross-section plot
#c=103.997/27.534 #center
c=103.918/27.588  #center
#az=42.5          #azimuth
az=55             #azimuth
baz=`echo $az | awk '{print $1+90}'`
l=-10/20
w=0/25          #define a box

#---Draw epicenter map and cross-section line
#Draw baemap
psbasemap -J${J} -R${R} -Y5.5i -B${B}:."Epicenter distribution":WSNE -P -K > ${psfile}
#Draw california coast, political boundaries, and water bodies
#pscoast -J${J} -R -Df -A10 -G200 -Ia -Na -P -K -O -V >> ${psfile}
#Draw km scale
pscoast -J -R -W1.0p,black -Lf104/27.37/27/30 -K -O >> ${psfile}
#Make cpt file
makecpt -Cwysiwyg -T0/20/0.1 -I -Z > g.cpt
#Draw epicenter map
awk '{if($17<5.1) print $3,$2,$4,$17*0.065}' ${locfile} | psxy -J -R -Cg.cpt -Sc -K -O >> ${psfile}
#draw main earthquake
awk '{if($1==7) print $3,$2,$4,$17*0.065}' ${locfile} | psxy -J -R -Cg.cpt -W0.5p,black -Sa -K -O >> ${psfile}
awk '{if($1==21) print $3,$2,$4,$17*0.065}' ${locfile} | psxy -J -R -Cg.cpt -W0.5p,black -Ss -K -O >> ${psfile}
# psxy -J -R -Sa -N -K -O << i >> ${psfile}
# 103.993 27.553 0.4
# 103.988 27.442 0.4
# i
#Draw focal mechanism
psmeca -J -R -Sc1 -C0.5pP2p -W1p -K -O << ! >> ${psfile}
103.993 27.553 12 350 51  35 236 63 136 2.858 24 103.90 27.49 201209071119  
103.988 27.442 12 234 58 155 338 69  34 1.103 24 103.90 27.44 201209071216  
!
# psmeca -J -R -Sc1 -C0.5pP2p -W1p -K -O << ! >> ${psfile}
# 103.993 27.553 12 236 63 136 350 51  35 2.858 24 103.90 27.49 201209071119  
# 103.988 27.442 12 234 58 155 338 69  34 1.103 24 103.90 27.44 201209071216  
# !
#Draw color scale
psscale -D6i/2i/3i/0.3i -B2:Depth:/:km: -Cg.cpt -E -K -O >> ${psfile}
#Draw earthquake size scale
psxy -J -R -Gblue -Sc -N -K -O << ! >> ${psfile}
103.84 27.73 0.065
103.86 27.73 0.13
103.88 27.73 0.195
103.90 27.73 0.26
103.92 27.73 0.325
103.94 27.73 0.39
!
pstext -J -R -Gblue -K -O << ! >> ${psfile}
103.82 27.71 10 0 1 2 M
103.84 27.71 10 0 1 2 1
103.86 27.71 10 0 1 2 2
103.88 27.71 10 0 1 2 3
103.90 27.71 10 0 1 2 4 
103.92 27.71 10 0 1 2 5 
103.94 27.71 10 0 1 2 6
!
#Draw nearby fault
psxy ylfault.txt -J -R -W1.0p,black -K -O >> ${psfile}
#Plot cross-section line on epicenter map
#cross-section A-A*, generally parallel to the fault
project -C${c} -A${az} -L${l} -G100 -Q -V > pro.tmp
awk '{print $1,$2}' pro.tmp | psxy -J -R -W1.0p,blue,- -K -O >> ${psfile}
awk '{if(NR==1) print $1,$2,15,0,0,"RB","A"}' pro.tmp | pstext -J -R -Gblack -K -O >> ${psfile}
awk 'END{print $1,$2,15,0,0,"LB","A*"}' pro.tmp | pstext -J -R -Gblack -N -K -O >> ${psfile}
#cross-section B-B*, generally perpendicular to the fault
project -C${c} -A${baz} -L${w} -G100 -Q -V > pro.tmp
awk '{print $1,$2}' pro.tmp | psxy -J -R -W1.0p,blue,- -N -K -O >> ${psfile}
awk '{if(NR==1) print $1,$2,15,0,0,"RB","B"}' pro.tmp | pstext -J -R -Gblack -K -O >> ${psfile}
awk 'END{print $1,$2,15,0,0,"LB","B*"}' pro.tmp | pstext -J -R -Gblack -K -O >> ${psfile}
#Get project data
awk '{print $3,$2,$4,$17,$1}' ${locfile} | project -C${c} -A${az}  -Q -V > proA.tmp
awk '{print $3,$2,$4,$17,$1}' ${locfile} | project -C${c} -A${baz} -Q -V > proB.tmp
#Plot hypocenters on cross-section
awk '{if($4<5.1) print $6,$3,$3,$4*0.065}' proA.tmp | \
psxy -JX3i/-2i -R${l}/0/20 -B5:"DIST(km)":/5:"Depth(km)"::."A-A*":WSne \
-Y-4i -Cg.cpt -Sc -K -O >> ${psfile}
awk '{if($5==7) print $6,$3,$3,$4*0.065}' proA.tmp | psxy -J -R -Cg.cpt -W0.5p,black -Sa -K -O >> ${psfile}
awk '{if($5==21) print $6,$3,$3,$4*0.065}' proA.tmp | psxy -J -R -Cg.cpt -W0.5p,black -Ss -K -O >> ${psfile}

awk '{if($4<5.1) print $6,$3,$3,$4*0.065}' proB.tmp | \
psxy -JX3i/-2i -R${w}/0/20 -B5:"DIST(km)":/5:"Depth(km)"::."B-B*":wSnE \
-X3.4i -Cg.cpt -Sc -K -O >> ${psfile}
awk '{if($5==7) print $6,$3,$3,$4*0.065}' proB.tmp | psxy -J -R -Cg.cpt -W0.5p,black -Sa -K -O >> ${psfile}
awk '{if($5==21) print $6,$3,$3,$4*0.065}' proB.tmp | psxy -J -R -Cg.cpt -W0.5p,black -Ss -K -O >> ${psfile}


rm *.tmp



                                                    
                                                