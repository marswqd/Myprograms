#!/bin/sh

#####
#	clean up
#####
rm -f *PLT
rm -f *egn
rm -f *dsp 
rm -f sdisp96*
rm -f *sac
rm -f tmp*
rm -f modl.in
rm -f modl.out
rm -f sobs.d


#####
#	create the velocity model interactively
#####
mkmod96 << EOF
simple.mod
Simple Crustal Model
0
40 6 3.5 2.7 0 0 0 0 1 1
0 8 4.7 3.3 0 0 0 0 1 1
EOF

#####
#	create the dfile use sprep96 -h for the format
#####
cat > dfile << EOF
2500 0.5 2048 0 8
EOF

#####
#	setup multimode surface wave dispersion and synthetics
#####
HS=10.0
HR=0.0
sprep96 -M simple.mod -d dfile -L -R -NMOD 100 -HS ${HS} -HR ${HR}
#####
#	get the dispersion and the eigenfunctions
#####
sdisp96
sregn96
slegn96
#####
#	make the synthetic for the station for a given focal mechanism and then convert to SAC
#####
DIP=45
RAKE=45 
STK=45
AZ=45
BAZ=225
MW=4.5
spulse96 -p -V -l 4 -d dfile | \
	fmech96 -D ${DIP} -R ${RAKE} -S ${STK} -A ${AZ} -ROT -MW ${MW} | \
	f96tosac -B
	

#####
#	convert synthetic output to meters/sec from cm/sec
#####
cat > 2.s << EOF
r *sac
div 100
w over
q
EOF
sac 2.s

#####
#	do interactive multiple filter analysis to get group velocity dispersion
#####

do_mft *.sac

#####
#	The disperions curves are in the files GRN21T.dsp and GRN21Z.dsp
#	combine these to create a SURF96 dispersion file
#####

MFTSRF *.dsp > disp.d

#####
#	get the theoretical dispersion for this model
#####
sdpegn96 -R -S -U -D disp.d -XLOG -PER
sdpegn96 -L -S -U -D disp.d -XLOG -PER

#####
#	Look at the observed and predicted disperion
#####
plotxvig < SREGNU.PLT
plotxvig < SLEGNU.PLT

#####
#	create a test model and then invert using surf96
#	note that we can not put this in the following interactive
#	script because of the way the CYGWIN disconnects subprocesses
#####

cat > sobs.d << EOF
  0.00499999989  0.00499999989  0.  0.00499999989  0.
    1    0    0    1    0    0    1    0    1    0
modl.in                                                                         
disp.d                                                                          
EOF
cat > modl.in << EOF
MODEL.01
surf96 inversion model
ISOTROPIC
KGS
FLAT EARTH
1-D
CONSTANT VELOCITY
LINE08
LINE09
LINE10
LINE11
  H(KM) VP(KM/S) VS(KM/S) RHO(GM/CC)   QP   QS  ETAP  ETAS  FREFP  FREFS
 10.0000  8.0000  4.7000  3.3000 0.00  0.00  0.00  0.00  1.00  1.00 
 10.0000  8.0000  4.7000  3.3000 0.00  0.00  0.00  0.00  1.00  1.00 
 10.0000  8.0000  4.7000  3.3000 0.00  0.00  0.00  0.00  1.00  1.00 
 10.0000  8.0000  4.7000  3.3000 0.00  0.00  0.00  0.00  1.00  1.00 
 10.0000  8.0000  4.7000  3.3000 0.00  0.00  0.00  0.00  1.00  1.00 
 10.0000  8.0000  4.7000  3.3000 0.00  0.00  0.00  0.00  1.00  1.00 
 10.0000  8.0000  4.7000  3.3000 0.00  0.00  0.00  0.00  1.00  1.00 
 10.0000  8.0000  4.7000  3.3000 0.00  0.00  0.00  0.00  1.00  1.00 
  0.0000  8.0000  4.7000  3.3000 0.00  0.00  0.00  0.00  1.00  1.00 
EOF

#####
#	now run the surface wave from the command line
#####

#####
#	fix the halfspace velocity
#####
surf96 31 9 0

#####
#	invert
#####
surf96 1 2 6 1 2 6 1 2 6
surf96 1 2 6 1 2 6 1 2 6
#####
#	save the model
#####
surf96 28 modl.out
#####
#	manually plot the fit to the dispersion and the resolution kernels
#####
srfphv96
srfphr96
plotxvig < SRFPHV96.PLT
plotxvig < SRFPHR96.PLT

#####
#	overlay the observed and predicted models
#####
shwmod96 -LEG -K -1 simple.mod modl.out
plotxvig < SHWMOD96.PLT

#####
#	ת����ʽ����
#####
plotnps -F7 -W10 -EPS -K < SHWMOD96.PLT > shwmod96.eps
convert -trim shwmod96.eps shwmod96.png





