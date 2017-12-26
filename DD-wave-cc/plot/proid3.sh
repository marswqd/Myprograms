# This script plots up a cross-section of seismicity data and an epicenter map
# data file is hypoDD's output file hypoDD.reloc and pickid's output file hypoDDloc.reloc
# compare the location of the events have same id in the file hypoDD.loc and hypoDD.reloc
# compare the location of the events have same id 
# in the file hypoDD-ct.reloc, hypoDD-cc.reloc and hypoDD-ctcc.reloc
#!/bin/sh

# gfortran pickid3.f90 -o pickid3
# ./pickid3

#Set parameters for plot
psfile=proid3.ps
ctloc=ct.reloc  #green
ccloc=cc.reloc  #blue
ctccloc=ctcc.reloc #red
R=103.8/104.2/27.4/27.75
B=f5ma10m
J=M5i
#Set parameters for cross-section plot
#c=103.997/27.534  #center
c=103.918/27.588  #center
#az=42.5           #azimuth 34.6
az=55             #azimuth 
baz=`echo $az | awk '{print $1+90}'`
l=-10/20
w=0/25          #define a box

#---Draw epicenter map and cross-section line
#Draw baemap
psbasemap -J${J} -R${R} -Y5.5i -B${B}:."Epicenter distribution":WSNE -P -K > ${psfile}
#Draw california coast, political boundaries, and water bodies
#pscoast -J${J} -R -Df -A10 -G200 -Ia -Na -K -O -V >> ${psfile}
#Draw km scale
pscoast -J -R -W1.0p,black -Lf104/27.37/27/30 -K -O >> ${psfile}
#Make cpt file
#makecpt -Cwysiwyg -T0/20/0.1 -I -Z > g.cpt
#Draw epicenter map
awk '{if($17<5.1) print $3,$2,$17*0.065}' ${ctloc} | psxy -J -R -Ggreen  -Sc -K -O >> ${psfile}
awk '{if($17<5.1) print $3,$2,$17*0.065}' ${ccloc} | psxy -J -R -Gblue -Sc -K -O >> ${psfile}
awk '{if($17<5.1) print $3,$2,$17*0.065}' ${ctccloc} | psxy -J -R -Gred -Sc -K -O >> ${psfile}
#draw main earthquake
awk '{if($1==7) print $3,$2,$17*0.065}' ${ctloc} | psxy -J -R -Ggreen -W0.5p,black -Sa -K -O >> ${psfile}
awk '{if($1==21) print $3,$2,$17*0.065}' ${ctloc} | psxy -J -R -Ggreen -W0.5p,black -Ss -K -O >> ${psfile}
awk '{if($1==7) print $3,$2,$17*0.065}' ${ccloc} | psxy -J -R -Gblue -W0.5p,black -Sa -K -O >> ${psfile}
awk '{if($1==21) print $3,$2,$17*0.065}' ${ccloc} | psxy -J -R -Gblue -W0.5p,black -Ss -K -O >> ${psfile}
awk '{if($1==7) print $3,$2,$17*0.065}' ${ctccloc} | psxy -J -R -Gred -W0.5p,black -Sa -K -O >> ${psfile}
awk '{if($1==21) print $3,$2,$17*0.065}' ${ctccloc} | psxy -J -R -Gred -W0.5p,black -Ss -K -O >> ${psfile}
# echo 103.993 27.553 0.4 | psxy -J${J} -R -Sa -K -O -N >> ${psfile} 
# echo 103.988 27.442 0.4 | psxy -J${J} -R -Sa -K -O -N >> ${psfile}
#Draw color scale
#psscale -Cg.cpt -D6i/2i/3i/0.3i -K -O -B2:Depth:/:km: -E >> ${psfile}
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
psxy -J -R -Ggreen -Sc -N -K -O << ! >> ${psfile}
104.11 27.73 0.39
!
psxy -J -R -Gblue -Sc -N -K -O << ! >> ${psfile}
104.11 27.71 0.39
!
psxy -J -R -Gred -Sc -N -K -O << ! >> ${psfile}
104.11 27.69 0.39
!
pstext -J -R -Ggreen -K -O << ! >> ${psfile}
104.15 27.73 15 0 1 CM cat
!
pstext -J -R -Gblue -K -O << ! >> ${psfile}
104.15 27.71 15 0 1 CM cc
!
pstext -J -R -Gred -K -O << ! >> ${psfile}
104.15 27.69 15 0 1 CM cat+cc
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
awk '{print $3,$2,$4,$17,$1}' ${ctloc} | project -C${c} -A${az}  -Q -V > proA1.tmp
awk '{print $3,$2,$4,$17,$1}' ${ctloc} | project -C${c} -A${baz} -Q -V > proB1.tmp
awk '{print $3,$2,$4,$17,$1}' ${ccloc} | project -C${c} -A${az}  -Q -V > proA2.tmp
awk '{print $3,$2,$4,$17,$1}' ${ccloc} | project -C${c} -A${baz} -Q -V > proB2.tmp
awk '{print $3,$2,$4,$17,$1}' ${ctccloc} | project -C${c} -A${az}  -Q -V > proA3.tmp
awk '{print $3,$2,$4,$17,$1}' ${ctccloc} | project -C${c} -A${baz} -Q -V > proB3.tmp
#Plot hypocenters on cross-section
awk '{if($4<5.1) print $6,$3,$4*0.065}' proA1.tmp | \
psxy -JX3i/-2i -R${l}/0/20 -B5:"DIST(km)":/5:"Depth(km)"::."A-A*":WSne \
-Y-4i -Ggreen -Sc -K -O >> ${psfile}
awk '{if($4<5.1) print $6,$3,$4*0.065}' proA2.tmp | psxy -J -R -Gblue -Sc -K -O >> ${psfile}
awk '{if($4<5.1) print $6,$3,$4*0.065}' proA3.tmp | psxy -J -R -Gred -Sc -K -O >> ${psfile}
awk '{if($5==7) print $6,$3,$4*0.065}' proA1.tmp | psxy -J -R -Ggreen -W0.5p,black -Sa -K -O >> ${psfile}
awk '{if($5==21) print $6,$3,$4*0.065}' proA1.tmp | psxy -J -R -Ggreen -W0.5p,black -Ss -K -O >> ${psfile}
awk '{if($5==7) print $6,$3,$4*0.065}' proA2.tmp | psxy -J -R -Gblue -W0.5p,black -Sa -K -O >> ${psfile}
awk '{if($5==21) print $6,$3,$4*0.065}' proA2.tmp | psxy -J -R -Gblue -W0.5p,black -Ss -K -O >> ${psfile}
awk '{if($5==7) print $6,$3,$4*0.065}' proA3.tmp | psxy -J -R -Gred -W0.5p,black -Sa -K -O >> ${psfile}
awk '{if($5==21) print $6,$3,$4*0.065}' proA3.tmp | psxy -J -R -Gred -W0.5p,black -Ss -K -O >> ${psfile}

awk '{if($4<5.1) print $6,$3,$4*0.065}' proB1.tmp | \
psxy -JX3i/-2i -R${w}/0/20 -B5:"DIST(km)":/5:"Depth(km)"::."B-B*":wSnE \
-X3.4i -Ggreen -Sc -K -O >> ${psfile}
awk '{if($4<5.1) print $6,$3,$4*0.065}' proB2.tmp | psxy -J -R -Gblue -Sc -K -O >> ${psfile}
awk '{if($4<5.1) print $6,$3,$4*0.065}' proB3.tmp | psxy -J -R -Gred -Sc -K -O >> ${psfile}
awk '{if($5==7) print $6,$3,$4*0.065}' proB1.tmp | psxy -J -R -Ggreen -W0.5p,black -Sa -K -O >> ${psfile}
awk '{if($5==21) print $6,$3,$4*0.065}' proB1.tmp | psxy -J -R -Ggreen -W0.5p,black -Ss -K -O >> ${psfile}
awk '{if($5==7) print $6,$3,$4*0.065}' proB2.tmp | psxy -J -R -Gblue -W0.5p,black -Sa -K -O >> ${psfile}
awk '{if($5==21) print $6,$3,$4*0.065}' proB2.tmp | psxy -J -R -Gblue -W0.5p,black -Ss -K -O >> ${psfile}
awk '{if($5==7) print $6,$3,$4*0.065}' proB3.tmp | psxy -J -R -Gred -W0.5p,black -Sa -K -O >> ${psfile}
awk '{if($5==21) print $6,$3,$4*0.065}' proB3.tmp | psxy -J -R -Gred -W0.5p,black -Ss -K -O >> ${psfile}


rm *.tmp

                                                    
                                                