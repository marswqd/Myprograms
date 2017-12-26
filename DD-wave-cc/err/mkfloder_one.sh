#!bin/bash
num=200  #重定位次数
loc=doloc #定位时使用的文件夹
mkdir ${loc}
mkdir reloc

for((i=1;i<=${num};i++));do
 file=`echo $i | awk '{printf("%03d",$1)}'`
 cc=dt.cc${file}
 echo ${cc}
 mv ${cc} ${loc}/
 cp event.dat ${loc}/
 cp sta_sel.txt ${loc}/
 cd ${loc}
cat > hypoDD.inp << EOF
hypoDD_2
* Parameter control file hypoDD.inp:
*
*---INPUT FILE SELECTION
* filename of cross-corr diff. time input (blank if not available):
${cc}
* filename of catalog travel time input (blank if not available):
dt.ct
* filename of initial hypocenter input:
event.dat
* filename of station input:
*station.dat
sta_sel.txt
*
*---OUTPUT FILE SELECTION
* filename of initial hypocenter output (if blank: output to hypoDD.loc):
hypoDD.loc
* filename of relocated hypocenter output (ifblank: output to hypoDD.reloc):
hypoDD.reloc
* filename of station residual output (if blank: no output written):
hypoDD.sta
* filename of data residual output (if blank: no output written):
hypoDD.res
* filename of takeoff angle output (if blank: no output written):
hypoDD.src
*
*
*---DATA SELECTION:
* IDAT: 1= cross corr; 2= catalog; 3= cross & cat 
* IPHA: 1= P; 2= S; 3= P&S
* DIST: max dist [km] between cluster centroid and station  
* IDAT IPHA DIST
    1    3   250
*
*---EVENT CLUSTERING:
* OBSCC:    min # of obs/pair for crosstime data (0= no clustering)
* OBSCT:    min # of obs/pair for network/cat data (0= no clustering)
* MINDIST:  min pair-station distance (-9= not active)
* MAXDIST:  max pair-station distance (-9= not active)
* MAXGAP:   max azimuthal gap (-9= not active)
* OBSCC  OBSCT  MINDS  MAXDS  MAXGAP 
    8      8    -999   -999   -999 
*
*---SOLUTION CONTROL:
* ISTART:  	1= from single source; 2= from network sources
* ISOLV:	1= SVD, 2= lsqr
* IAQ:		remove airquakes: 0= NO, 1= YES
* NSET:     number of sets of iteration with specifications following
* ISTART  ISOLV  IAQ  NSET
     2      2     0     5 
*
*---DATA WEIGHTING AND REWEIGHTING:
* NITER: 		last iteration to used the following weights
* WTCCP, WTCCS:	weight cross P, S 
* WTCTP, WTCTS:	weight catalog P, S 
* WRCC,  WRCT:	residual threshold in sec for cross, catalog data 
* WDCC,  WDCT:  max dist [km] between cross, catalog linked pairs
* DAMP:    		damping (for lsqr only) 
*          ---- CROSS DATA ----      ---- CATALOG DATA ---- 
* NITER  WTCCP  WTCCS  WRCC  WDCC    WTCTP  WTCTS  WRCT  WDCT  DAMP
    5      1.0   0.5    -9    10      1.0   0.5     -9    20    140
    5      1.0   0.5     6     8      1.0   0.5      4     8    140
    5      1.0   0.5     6     8      1.0   0.5      4     8    140
    5      1.0   0.5     4     6      1.0   0.5      4     6    140
    5      1.0   0.5     4     4      1.0   0.5      4     4    140
* 
*---FORWARD MODEL SPECIFICATIONS:
* IMOD  Specification of forward model (imod = 0 was used in hypoDD version 1): 
*       0= 1D layered P-velocity model with constant Vp/Vs ratio (e.g. hypoDD 1).
*       1= 1D layered P-velocity model with variable Vp/Vs ratio.
*       4= Station specific 1D layered velocity models.
*       5= Straight ray path (i.e. constant velocity). Allows for negative station
*          elevations, including stations locations below sources (e.g. borehole networks).
*       9= 3D P-and S-velocity model using the pseudo-bending ray tracing algorithm from the simul2000
*
* IMOD
0
*** If IMOD=0:
* NLAY  RATIO : number of model layers (max 30); vp/vs ratio
5 1.73
* TOP: depths of top of layer (km)
0.0 2.0 8.0 16.0 24.0 32.0 46.0
* VELP: layer P velocities (km/s)
5.5 5.75 6.0 6.3 5.6 6.7 7.7
*
*** If IMOD=1 or IMOD=5:
* TOP: depths of top of layer (km)
*0.00  3.00  10.0  20.0  32.0  36.0  40.0 45.0 
* VELP: layer P velocities (km/s)
*5.169 5.996 6.008 6.344 6.408 6.345 6.80 7.90 
* RATIO: layer ratios
*1.824 1.770 1.770 1.767 1.768 1.767 1.76 1.816
* If IMOD=5, only first value is taken.
*
*** If IMOD=4:
* 1DMOD: Filename of 1D models linked to stations in station file
* models1D.dat
* Leave two lines empty:
* EMPTYLINE
* EMPTYLINE
**
*** If IMOD=9:
* 3DMOD: name of 3D velocity model
* vel3d.dat
* LAT3D  LON3D  ROT3D: origin of 3D velocity model and rotation of cart coord. system (pos= anti clockwise)
* 37.5  -122.20 0.0
* IPHA  NDIP  ISKIP  SCALE1  SCALE2  XFAX  TLIM  NITPB: raytracing parameters
* 2 9 2 1 1 1.35  0.0005 50
*
*---CLUSTER/EVENT SELECTION:
* CID: cluste id to be relocated (0 = all)
1 
* ID: cuspids of event to be relocated (8 per line)

* end of hypoDD.inp 
EOF
 hypoDD hypoDD.inp
 cp hypoDD.reloc ../reloc/${file}.reloc
 cd ..
done








