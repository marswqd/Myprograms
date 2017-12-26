#!bin/sh
#对台站名相同,位置不同的台站sac文件进行操作,使两者区分开来

# CAD:
# 201103之前 为：
 # CAD     31.1000004       97.1699982       3349.00000
# 201103之后 为 
 # CAD     31.1399994       97.1699982       3338.00000
 
# LIZ:
# 201103之前 为：
 # LIZ     29.6700001       94.3399963       2994.00000
# 201103之后 为 
 # LIZ     29.7000008       94.3399963       2995.00000
 

# year=( 2009 2010 2011 2012 2013 );nyear=${#year[@]}
# mon=( 01 02 03 04 05 06 07 08 09 10 11 12 );nmon=${#mon[@]}

#sta=( CAD LIZ );nsta=${#sta[@]};echo $nsta
#stla=( 31.1399994 29.7000008 )
#stlo=( 97.1699982 94.3399963 )
#stel=( 3338.00000 2995.00000 )

# sta=( BIJ ERY GAX LEB PUG TEC WIN XIC YAY );nsta=${#sta[@]};echo $nsta
# sta2=( BJT EYA HWS LBO PGE TNC WNT XCE YYU )
# stla=( 27.243500 26.108801 28.636000 28.272200 27.383900 25.028900 26.907101 28.941700 27.466900)
# stla2=( 27.243500 26.108801 28.639601 28.272200 27.383900 25.028900 26.907200 28.941700 27.466900)
# stlo=( 105.349998 99.947502 104.735901 103.570300 102.542400 98.519798 104.303001 99.791702 101.677597 )
# stlo2=( 105.349700 99.947502 104.735900 103.570297 102.542397 98.519798 104.302900 99.791702 101.677600 )
# stel=( 1463.000000 2063.000000 860.000000 1310.000000 1427.000000 1615.000000 2334.000000 3000.000000 2596.000000 )
# stel2=( 1464.000000 2063.000000 860.000000 1310.000000 1427.000000 1615.000000 2334.000000 3000.000000 2596.000000 )

year=( 2009 );nyear=${#year[@]}
mon=( 07 08 );nmon=${#mon[@]}
sta=( 531 532 533 534 535 536 );nsta=${#sta[@]};echo $nsta
sta2=( B31 B32 B33 B34 B35 B36 )

for((j=0;j<nyear;j++));do
 for((i=0;i<nmon;i++));do
  #file=${year}${mon[i]}
  echo ${year[j]}${mon[i]}
  #mkdir ${year}${mon[i]}
  cd ${year[j]}/${year[j]}${mon[i]}/sac
  pwd
  #mkdir evt kun sac
  for((k=0;k<nsta;k++));do
   file=(`find ${sta[k]}.*.SAC -prune`);nfile=${#file[@]}   #;echo $nfile  
   #flag=a
   flag=`echo $nfile | awk '{if($1>=1) {print "b"} else {print "a"}}'`   
   if [[ ${flag} = b ]];then
    echo $nfile
    #mkdir ${sta[k]}-${sta2[k]}
	#mv ${sta[k]}.*.SAC ${sta[k]}-${sta2[k]}/
	#cd ${sta[k]}-${sta2[k]}
	#rename ${sta[k]} ${sta2[k]} ${sta[k]}.*.SAC
	#for((n=0;n<nfile;n++));do
     #f=${file[n]:2}
	 #f2=${f/${sta[k]}/${sta2[k]}}
	 #echo $f $f2
	 #cp $f $f2
# sac << EOF
# r $f2
# lh KSTNM STLA STLO STEL
# ch KSTNM ${sta2[k]} STLA ${stla2[k]} STLO ${stlo2[k]} STEL ${stel2[k]}
# wh
# lh KSTNM STLA STLO STEL
# w over
# q
# EOF
     #cp $f2 ../
	#done
	#cd ..
   else
    echo "No file for ${sta[k]}"
   fi
  done  
  cd ../../..
 done
done









