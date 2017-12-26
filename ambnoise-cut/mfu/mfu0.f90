!功能: 多重滤波提取群速度+瞬时相位、瞬时周期、信噪比计算
!      计算特定周期的群速度，输出滤波波形和包络，用于检视alpha值是否合适
!      连续提取群速度请使用mfu1.f90
!作者: 王清东  时间: 2011-05-02 14:33:44
program mfu
 implicit none

 integer*4::i,j,k,lengh,IRU,nerr,num,nt,NPTS,pl,ph,PNPTS
 real*4::snr,gv,ip,it,O
 real*4,allocatable::TC(:),alpha(:)
 real*4,parameter::plow=2.0e0,phigh=5.0e0      !绘图窗(信号窗)
 character*1::dir
 character*60::wname,wnameE
 character*60,allocatable::sacname(:)

 real*4,allocatable::data(:),dataE(:),dataIP(:,:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)

 open(10,file='mfu0.out')                    !程序运行信息文件

! 读取控制文件信息
 open(11,file='mfu0.in',status='old')
 read(11,*) num,dir,nt
 !dir=D:输入的是双端信号
 !dir=-:输入的是非因果信号                  !这里的非因果指的是B<0,E=0的情况
 !dir=+:输入的是因果信号
 allocate(sacname(num),TC(nt),alpha(nt))    !nt:滤波中心周期数 alpha:滤波中心周期对应的滤波参数数
 read(11,*) (TC(i),i=1,nt)
 read(11,*) (alpha(i),i=1,nt)
 do i=1,num
  read(11,*) sacname(i)
  call filesta(sacname(i),nerr)
  if(nerr.eq.1) then
   print*, 'The file ',trim(sacname(i)),' dosen`t exist'
   goto 99
  endif
  sacname(i)=adjustl(sacname(i))
  print*, trim(sacname(i))
 enddo
 close(11)

 IRU=100
 !write(10,*) 'alpha =',alpha
 !write(10,*) '------------------------------------------------------------------'
!  DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
!  STLA=rhdr(32)  STLO=rhdr(33)  STEL=rhdr(34)  STDP=rhdr(35)
!  EVLA=rhdr(36)  EVLO=rhdr(37)  EVEL=rhdr(38)  EVDP=rhdr(39)
!  NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
!  NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)  KSTNM=chdr(1)
 do k=1,num
  call brsach(IRU,sacname(k),nerr)           !读SAC文件头，提取必要信息
  print*, 'dist= ',rhdr(51)
  lengh=len(trim(sacname(k)))
  !print*, 'lengh=',lengh
  NPTS=ihdr(10)
  !对互相关文件,发震时刻O为0
  O=rhdr(8)
  !B=rhdr(6)
  if(O.eq.-12345.0e0) then
   O=0.0e0
   print*, 'the orign time of',sacname(i),'is unknown'
  endif
  ph=anint((rhdr(51)/plow-rhdr(6)+O)/rhdr(1))+1     !确定绘图窗(信号窗)
  pl=anint((rhdr(51)/phigh-rhdr(6)+O)/rhdr(1))+1
  PNPTS=ph-pl+1
  !print*, 'ph=',ph,'pl=',pl
  allocate(data(NPTS),dataE(NPTS),dataIP(NPTS,2))
  write(10,*) trim(sacname(k))
  write(10,110)
  110 format(10x'滤波后文件'10x'震中距'10x'中心周期'10x'alpha'10x'信噪比' &
             10x'群速度'8x'瞬时相位(°)'5x'瞬时周期'2x'1:因果 -1:非因果')
  do j=1,nt
   call brsac(IRU,NPTS,sacname(k),data,nerr) !读取波形数据
! 多重滤波,得到包络和瞬时频率
   call MFT(data,NPTS,rhdr(1),TC(j),alpha(j),dataE,dataIP)
   wname=sacname(k)
   wname(lengh+1:lengh+1)='.'
   if(TC(j).lt.10.0e0) then
    wname(lengh+2:lengh+2)='0'
    write(wname(lengh+3:lengh+3),'(i1)') int(TC(j))
   else if(TC(j).lt.100.0e0) then
    write(wname(lengh+2:lengh+3),'(i2)') int(TC(j))
   endif
   wname(lengh+4:lengh+5)='.A'
   if(dir.eq.'d'.or.dir.eq.'D') then
! 计算因果信号的信噪比
    call SUF(data,dataE,dataIP,NPTS,TC(j),rhdr(6),rhdr(1),rhdr(51),1,snr,gv,ip,it,nerr)
    write(10,*) wname(1:lengh+5),rhdr(51),TC(j),alpha(j),snr,gv,ip,it,' 1'
! 计算非因果信号的信噪比
    call SUF(data,dataE,dataIP,NPTS,TC(j),rhdr(6),rhdr(1),rhdr(51),-1,snr,gv,ip,it,nerr)
    write(10,*) wname(1:lengh+5),rhdr(51),TC(j),alpha(j),snr,gv,ip,it,'-1'
   else if(dir.eq.'-') then
    call SUF(data,dataE,dataIP,NPTS,TC(j),rhdr(6),rhdr(1),rhdr(51),-1,snr,gv,ip,it,nerr)
    write(10,*) wname(1:lengh+5),rhdr(51),TC(j),alpha(j),snr,gv,ip,it,'-1'
   else if(dir.eq.'+') then
    call SUF(data,dataE,dataIP,NPTS,TC(j),rhdr(6),rhdr(1),rhdr(51),1,snr,gv,ip,it,nerr)
    write(10,*) wname(1:lengh+5),rhdr(51),TC(j),alpha(j),snr,gv,ip,it,' 1'
   else
    print*, 'The dir must be D or - or +, please check'
   endif
! 生成滤波后的文件
   print*, trim(wname)
   call nor(data,NPTS)
   call wsac(data,NPTS,wname)
   wnameE=wname
   wnameE(lengh+5:lengh+5)='E'
   print*, trim(wnameE)
   call wsac(dataE,NPTS,wnameE)
  enddo
  write(10,*) '------------------------------------------------------------------'
 deallocate(data,dataE,dataIP)
 enddo

 99 continue
 deallocate(alpha,TC,sacname)
 close(10)
end program


subroutine filesta(name,nerr)
 implicit none
 integer*4::nerr
 character*(*)::name
 logical::alive
 nerr=0
 inquire(file=name,exist=alive)
 if(alive.eqv..false.) then
  !print*, 'The file ',trim(name),' dosen`t exist'
  nerr=1
 endif
 return
end subroutine


subroutine brsach(IRU,name,nerr)
!-----
!       IRU I*4 logical unit for IO
!       name    C*  Name of file to be opened
!       rhdr    R*4 Real header
!       ihdr    I*4 Integer Header
!       chdr    C*  Character Header
!       nerr    I*4 -1 file does not exist
!               -2 data points in file exceed dimension
!
!       NOTE IF THE BINARY FILE HAS MORE THAN LN POINTS, THEN ONLY
!       LN POINTS ARE USED
!-----
!  This routine reads waveform data written in SAC binary format.
!
!  Written by Hafidh A. A. Ghalib, 1988.
!-----
 implicit none
 integer*4::i,IRU,nerr
 logical::ext
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
!-----
!  Read real and integer header blocks to find actual number
!  of waveform data points which is stored in ihdr(10).
!-----
 inquire(file=name,exist=ext)
 if(.not.ext) then
  ihdr(10)=0
  nerr=-1
  return
 endif
 nerr=0
 !open(IRU,file=name,form='unformatted',access='direct',recl=440,status='old')
 !read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40)
 open(IRU,file=name,form='unformatted',access='direct',recl=632,status='old')
 read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40),(chdr(i),i=1,24)
 close(IRU)
 return
end subroutine


subroutine brsac(IRU,LN,name,data,nerr)
!-----
!       IRU I*4 logical unit for IO
!       LN  I*4 length of data array
!       name    C*  Name of file to be opened
!       rhdr    R*4 Real header
!       ihdr    I*4 Integer Header
!       chdr    C*  Character Header
!       data    R*4 Array of trace values
!       nerr    I*4 -1 file does not exist
!               -2 data points in file exceed dimension
!
!       NOTE IF THE BINARY FILE HAS MORE THAN LN POINTS, THEN ONLY
!       LN POINTS ARE USED
!-----
!  This routine reads waveform data written in SAC binary format.
!
!  Written by Hafidh A. A. Ghalib, 1988.
!-----
 implicit none
 integer*4::i,IRU,LN,nerr,maxpts,nbytes,ndat,nread,ndat1,irec,nl,nh
 real*4::data(LN)
 logical::ext
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
!  Read real and integer header blocks to find actual number
!  of waveform data points which is stored in ihdr(10).
! print*, name
 inquire(file=name,exist=ext)
 if(.not.ext) then
  ihdr(10)=0
  nerr=-1
  return
 endif
 nerr=0
 open(IRU,file=name,form='unformatted',access='direct',recl=440,status='old')
 read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40)
 close(IRU)
!  Read header and waveform data blocks using recored length of 158*4=632.
 if(ihdr(10).gt.LN) then
  maxpts=LN
  ihdr(10)=LN
  nerr=-2
 else
  maxpts=ihdr(10)
  nerr=0
 endif
 nbytes=632+4*maxpts
 nread=0
!  because of SUNOS Fortran problems with IO transfers
!  more than 2048 bytes, read these  chunks in
 ndat=maxpts
 if(nbytes.gt.2048) then
  open(IRU,file=name,form='unformatted',access='direct',recl=2048)
  ndat1=(2048-632)/4
  irec=1
  read(IRU,rec=irec,err=1001) (rhdr(i),i=1,70),   &
                              (ihdr(i),i=1,40),   &
                              (chdr(i),i=1,24),   &
                              (data(i),i=1,ndat1)
  nread=nread+ndat1
  1000 continue
  nl=nread+1
  nh=nl+512-1
  if(nh.gt.ndat) then
   nh=ndat
  endif
  if(nl.gt.ndat) goto 1001
  irec=irec+1
  read(IRU,rec=irec,err=1001) (data(i),i=nl,nh)
  nread=nread+(nh-nl+1)
  goto 1000
  1001 continue
  close(IRU)
 else
  open(IRU,file=name,form='unformatted',access='direct',recl=nbytes)
  read(IRU,rec=1) (rhdr(i),i=1,70),   &
                  (ihdr(i),i=1,40),   &
                  (chdr(i),i=1,24),   &
                  (data(i),i=1,ndat)
  close(IRU)
 endif
 if(ihdr(10).gt.LN) then
  maxpts=LN
  ihdr(10)=LN
 else
  maxpts=ihdr(10)
 endif
  ihdr(10)=maxpts
 return
end subroutine


subroutine taper(data,n,width)
!仿照sac2000中的taper命令
!DATA(J)=DATA(J)*(F0-F1*COS(OMEGA*(J-1))
!======== ========= ===== ======
!TYPE     OMEGA     F0    F1
!======== ========= ===== ======
!HANNING   PI/N     0.50  0.50
!HAMMING   PI/N     0.54  0.46
!COSINE    PI/(2*N) 1.00  1.00
!======== ========= ===== ======
!这里选用HANNING
 implicit none
 integer*4::i,j,k,n,tl,tr
 real*4::data(n),width,f0,f1,omega
 real*4,parameter::pi=3.14159265358e0
 do i=1,n
  if(data(i).ne.0.0e0) exit
 enddo
 if(i.eq.n) return
 tl=i
 do i=n,1,-1
  if(data(i).ne.0.0e0) exit
 enddo
 tr=i
 f0=0.5e0
 f1=0.5e0
 j=anint((tr-tl+1)*width)
 omega=pi/j
 do i=tl,tl+j-1
  data(i)=data(i)*(f0-f1*cos(omega*(i-tl)))
 enddo
 do i=tr,tr-j+1,-1
  data(i)=data(i)*(f0-f1*cos(omega*(i-tr)))
 enddo
 return
end subroutine


subroutine SUF(data,dataE,dataIP,n,TC,B,DT,DIST,dir,snr,gv,ip,it,nerr)
! dir=1 : 计算因果信号的信噪比
! dir=-1: 计算非因果信号的信噪比
 implicit none
 integer*4::i,n,dir,sl,sr,nl,nr,maxI,nerr
 real*4::data(n),dataE(n),dataIP(n,2),TC,B,DT,DIST,maxE
 real*4::speak,nrms,srms,snr,gv,ip,it
 real*4,parameter::Umin=2.0e0,Umax=5.0e0
 !real*4,parameter::Tmin=1.0e0,Tmax=50.0e0
 nerr=0
 if(dir.ne.1.and.dir.ne.-1) then
  nerr=1
  return
 endif
 !sl=floor(((DIST/Umax-Tmax)*dir-B)/DT)
 sl=floor((DIST/Umax*dir-B)/DT)
 !if(sl.lt.1) sl=1
 !sr=floor(((DIST/Umin+2*Tmin)*dir-B)/DT)+1
 sr=floor((DIST/Umin*dir-B)/DT)+1
 !print*, sl,sr,dir
 speak=0.0e0
 maxE=0.0e0
 do i=sl,sr,dir
  if(speak.lt.abs(data(i))) speak=abs(data(i))
  if(maxE.lt.dataE(i)) then
   maxE=dataE(i)
   maxI=i
  endif
 enddo
 !print*, maxI
 gv=DIST/(B+(maxI-1)*DT)*dir            !群速度
 ip=dataIP(maxI,1)                      !瞬时相位
 it=dataIP(maxI,2)                      !瞬时周期
 nl=floor(-B/DT)+1
 nr=maxI-anint(1.5e0*TC/DT)*dir
 !print*, nl,nr,dir
 if((nr-maxI)*(nr-nl).gt.0) then
  snr=-99999.0e0
  return
 endif
 srms=0.0e0
 sl=nr
 sr=maxI+anint(1.5e0*TC/DT)*dir
 do i=sl,sr,dir
  srms=srms+data(i)**2
 enddo
 srms=sqrt(srms/((sr-sl)*dir+1))
 nrms=0.0e0
 do i=nl,nr,dir
  nrms=nrms+data(i)**2
 enddo
 nrms=sqrt(nrms/((nr-nl)*dir+1))
 snr=speak/nrms                       !信噪比
 !snr=srms/nrms
 return
end subroutine


subroutine MFT(data,n,dt,TC,alpha,dataE,dataIP)
! 多重滤波程序(窄带滤波器为高斯滤波器,截断为pai=3.14159265357e0)
! data:输入输出-波形数据
! dataE:输出-波形振幅(包络)
! dataIP:输出-(瞬时相位,瞬时周期)
 implicit none
 integer*4::i,n,logn,nfft
 real*4::data(n),dataE(n),dataIP(n,2)
 real*4::dt,TC,fc,df,f,alpha,fac,freqlw,frequp,G,s
 real*4,parameter::pai=3.14159265358e0
 real*4,allocatable::PR(:),PI(:),FR(:),FI(:),PRD(:),PID(:),FRD(:),FID(:)
 call npow2(n,nfft,logn)
 fc=1.0e0/TC
 df=1.0e0/(nfft*dt)
 !print*, 'logn=',logn,'nfft=',nfft
 allocate(PR(nfft),PI(nfft),FR(nfft),FI(nfft))
 allocate(PRD(nfft),PID(nfft),FRD(nfft),FID(nfft))
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 PRD=0.0e0
 PID=0.0e0
 FRD=0.0e0
 FID=0.0e0
 call taper(data,n,0.005)
 PR(1:n)=data(1:n)
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,0)
 fac=sqrt(pai/alpha)
 frequp=(1.0e0+fac)*fc
 freqlw=(1.0e0-fac)*fc
 do i=1,nfft/2+1
  f=(i-1)*df
  s=2*pai*f
  if(f.ge.freqlw.and.f.le.frequp) then
   G=exp(-alpha*(f/fc-1.0e0)**2)
  else
   G=0.0e0
  endif
  PR(i)=FR(i)*G
  PI(i)=FI(i)*G
  PRD(i)=-s*PI(i)
  PID(i)=s*PR(i)
 enddo
 PR(1)=PR(1)/2
 PI(1)=PI(1)/2
 PR(nfft/2+1)=PR(nfft/2+1)/2
 PI(nfft/2+1)=PI(nfft/2+1)/2
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !令负频率为零
 PRD(1)=PRD(1)/2
 PID(1)=PID(1)/2
 PRD(nfft/2+1)=PRD(nfft/2+1)/2
 PID(nfft/2+1)=PID(nfft/2+1)/2
 PRD(nfft/2+2:nfft)=0.0e0
 PID(nfft/2+2:nfft)=0.0e0       !令负频率为零
 call taper(PR,nfft,0.005)
 call taper(PI,nfft,0.005)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,1)
 data(1:n)=2*FR(1:n)
 dataE(1:n)=PR(1:n)
 dataIP(1:n,1)=PI(1:n)!*2*pai/360.0e0
 call taper(PRD,nfft,0.005)
 call taper(PID,nfft,0.005)
 call KKFFT(PRD,PID,nfft,logn,FRD,FID,1,0)
 do i=1,n
  dataIP(i,2)=(FR(i)*FID(i)-FRD(i)*FI(i))/(dataE(i)**2)
  dataIP(i,2)=2*pai/dataIP(i,2)
 enddo
 deallocate(PR,PI,FR,FI,PRD,PID,FRD,FID)
 return
end subroutine


SUBROUTINE KKFFT(PR,PI,N,K,FR,FI,L,IL)
!L=0 for fft;L=1 for ifft
!IL=0 不计算模与幅角;IL=1 计算模与幅角
 IMPLICIT NONE
 INTEGER*4::I,N,K,IT,L,IL,M,IS,J,NV,L0
 REAL*4::PR(N),PI(N),FR(N),FI(N),P,Q,S,VR,VI,PODDR,PODDI
 DO IT=0,N-1
  M=IT
	IS=0
	DO I=0,K-1
	 J=M/2
	 IS=2*IS+(M-2*J)
	 M=J
  ENDDO
	FR(IT+1)=PR(IS+1)
	FI(IT+1)=PI(IS+1)
 ENDDO
 PR(1)=1.0E0
 PI(1)=0.0E0
 PR(2)=COS(6.283185306E0/N)
 PI(2)=-SIN(6.283185306E0/N)
 IF(L.NE.0) PI(2)=-PI(2)
 DO I=3,N
	P=PR(I-1)*PR(2)
	Q=PI(I-1)*PI(2)
	S=(PR(I-1)+PI(I-1))*(PR(2)+PI(2))
	PR(I)=P-Q
	PI(I)=S-P-Q
 ENDDO
 DO IT=0,N-2,2
	VR=FR(IT+1)
	VI=FI(IT+1)
	FR(IT+1)=VR+FR(IT+2)
	FI(IT+1)=VI+FI(IT+2)
	FR(IT+2)=VR-FR(IT+2)
	FI(IT+2)=VI-FI(IT+2)
 ENDDO
 M=N/2
 NV=2
 DO L0=K-2,0,-1
	M=M/2
	NV=2*NV
	DO IT=0,(M-1)*NV,NV
	 DO J=0,(NV/2)-1
	  P=PR(M*J+1)*FR(IT+J+1+NV/2)
	  Q=PI(M*J+1)*FI(IT+J+1+NV/2)
	  S=PR(M*J+1)+PI(M*J+1)
	  S=S*(FR(IT+J+1+NV/2)+FI(IT+J+1+NV/2))
	  PODDR=P-Q
	  PODDI=S-P-Q
	  FR(IT+J+1+NV/2)=FR(IT+J+1)-PODDR
    FI(IT+J+1+NV/2)=FI(IT+J+1)-PODDI
	  FR(IT+J+1)=FR(IT+J+1)+PODDR
	  FI(IT+J+1)=FI(IT+J+1)+PODDI
   ENDDO
  ENDDO
 ENDDO
 IF(L.NE.0) THEN
  DO I=1,N
	 FR(I)=FR(I)/N
	 FI(I)=FI(I)/N
  ENDDO
 ENDIF
 IF(IL.NE.0) THEN
  DO I=1,N
	 PR(I)=SQRT(FR(I)*FR(I)+FI(I)*FI(I))
   PI(I)=ATAN(FI(I)/FR(I))*360.0E0/6.283185306E0
   !将相位还原为[0,2pai],即[0,360]
   !IF(FR(I).LT.0.0E0) PI(I)=180.0E0+PI(I)
   !IF(FI(I).LE.0E0.AND.FR(I).GE.0.0E0) PI(I)=360.0E0+PI(I)
  ENDDO
 ENDIF
 RETURN
END SUBROUTINE


subroutine wsac(data,n,wname)
 implicit none
 integer*4::i,n,len
 real*4::min,max,sum
 real*4::data(n)
 character*(*)::wname
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 ihdr(10)=n
 min=1.0e+38
 max=-1.0e+38
 sum=0.0e0
 do i=1,ihdr(10)
	if(min.gt.data(i)) min=data(i)
	if(max.lt.data(i)) max=data(i)
  sum=sum+data(i)
 enddo
 rhdr(2)=min
 rhdr(3)=max
 rhdr(57)=sum/ihdr(10)
!  NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
!  NZSEC=ihdr(5)  NZMSEC=ihdr(6)
!  ihdr(1)=year
!  ihdr(2)=day
 !print*, wname
 len=len_trim(wname)
 call bwsac(20,ihdr(10),wname(1:len),data)
 return
end subroutine


subroutine bwsac(IWU,LN,name,data)
!-----
!  This routine writes out a waveform data in SAC binary format.
!
!  Written by Hafidh A. A. Ghalib, 1988.
!-----
 implicit none
 integer*4::i,k,j,l,IWU,LN,nerr,nrec
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
 real*4::data(LN)
!-----
!  remove the original file so that the output length is
!  never greater than desired. Else the dregs of the
!  first will remain
!-----
 open(IWU,file=name,form='unformatted',access='sequential',status='unknown')
 rewind(IWU)
 close(IWU,status='delete')
!  The actual number of waveform data points is stored in integer
!  header 10. The file recored length is 158*4=632.
 nrec=632+4*ihdr(10)
 open(IWU,file=name,form='unformatted',access='direct',recl=nrec,status='unknown')
 write(IWU,rec=1) (rhdr(i),i=1,70),     &
                  (ihdr(k),k=1,40),     &
                  (chdr(j),j=1,24),     &
                  (data(l),l=1,ihdr(10))
 close(IWU)
 return
end subroutine


subroutine npow2(n,nfft,logn)
 implicit none
 integer*4::n,nfft,logn
 nfft=1
 logn=0
 do while(nfft.lt.n)
  nfft=2*nfft
  logn=logn+1
 enddo
 return
end subroutine


subroutine nor(data,n)
 implicit none
 integer*4::n,i
 real*4::data(n),max
 max=0.0e0
 do i=1,n
  if(max.lt.abs(data(i))) max=abs(data(i))
 enddo
 data=data/max
end subroutine

