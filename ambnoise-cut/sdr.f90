!本程序的功能是：依据距离(dist)和信噪比(SNR)来选取(selection)满足条件的互相关文件
!作者：王清东 时间：2011-02-24 15:49:16
program sdr
 implicit none

 integer*4::i,IRU,nerr,num,NPTS,dir
 real*4::dist,SNR,alpha,TC,ratio,gv,ip,it,T1,T2
 character*60::wname
 character*60,allocatable::sacname(:)

 real*4,allocatable::data(:),dataE(:),dataIP(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)

 call CPU_TIME(T1)
 open(10,file='infoall.txt')                   !程序运行信息文件
 open(11,file='infoselect.txt')

!读取控制文件信息
 open(12,file='sdrfile',status='old')       !sdrfile中存放需要进行操作的文件信息
 read(12,*) num,dist,SNR
 allocate(sacname(num))                     !sacname:sac文件名
 do i=1,num
  read(12,*) sacname(i)
  sacname(i)=adjustl(sacname(i))
  print*, trim(sacname(i))
 enddo
 close(12)

 IRU=100
 alpha=6.25e0
 do i=1,num
!  DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
!  STLA=rhdr(32)  STLO=rhdr(33)  STEL=rhdr(34)  STDP=rhdr(35)
!  EVLA=rhdr(36)  EVLO=rhdr(37)  EVEL=rhdr(38)  EVDP=rhdr(39)
!  NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
!  NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)  KSTNM=chdr(1)
  call brsach(IRU,sacname(i),nerr)
  NPTS=ihdr(10)
  allocate(data(NPTS))
  allocate(dataE(NPTS))
  allocate(dataIP(NPTS,2))
  call brsac(IRU,NPTS,sacname(i),data,nerr)
! 多重滤波,得到包络和瞬时频率
  TC=rhdr(51)/10.0e0
  call MFT(data,NPTS,rhdr(1),TC,alpha,dataE,dataIP)
! 计算因果信号的信噪比
  dir=1
  call SUF(data,dataE,dataIP,NPTS,rhdr(6),rhdr(1),rhdr(51),dir,ratio,gv,ip,it,nerr)
   if(nerr.ne.0) then
    write(10,*) 'The NPTS of ',trim(sacname(i)),' is too few, please check'
    write(10,*) '------------------------------------------------------------------'
    print*, 'The NPTS of ',trim(sacname(i)),' is too few, please check'
    goto 99
  endif
  write(10,*) trim(sacname(i)),rhdr(51),ratio
  if(rhdr(51).ge.dist.and.ratio.ge.SNR) then
   write(11,*) trim(sacname(i)),rhdr(51),ratio
  endif
  deallocate(data)
  deallocate(dataE)
  deallocate(dataD)
 enddo

 99 continue
 deallocate(sacname)
 close(10)
 close(11)
 call CPU_TIME(T2)
 print*, 'T=',T2-T1
end program


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


subroutine SUF(data,dataE,dataIP,n,B,DT,DIST,dir,ratio,gv,ip,it,nerr)
! dir=1 : 计算因果信号的信噪比
! dir=-1: 计算非因果信号的信噪比
 implicit none
 integer*4::i,n,dir,sl,sr,nl,nr,maxI,nerr
 real*4::data(n),dataE(n),dataIP(n,2),B,DT,DIST,maxE
 real*4::speak,nrms,ratio,gv,ip,it
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
 nl=sr+anint(500.0e0/DT)*dir
 if(nl.gt.n.or.nl.lt.1) then
  nerr=1
  return
 endif
 nr=nl+anint(500.0e0/DT)*dir
 if(nr.gt.n) nr=n
 if(nr.lt.1) nr=1
 !print*, sl,sr
 speak=0.0e0
 maxE=0.0e0
 do i=sl,sr,dir
  if(speak.lt.abs(data(i))) speak=abs(data(i))
  if(maxE.lt.dataE(i)) then
   maxE=dataE(i)
   maxI=i
  endif
 enddo
 gv=DIST/(B+(maxI-1)*DT)*dir
 ip=dataIP(maxI,1)
 it=dataIP(maxI,2)
 nrms=0.0e0
 do i=nl,nr,dir
  nrms=nrms+data(i)**2
 enddo
 nrms=sqrt(nrms/((nr-nl)*dir+1))
 ratio=speak/nrms
 return
end subroutine


subroutine MFT(data,n,dt,TC,alpha,dataE,dataIP)
! 多重滤波程序(窄带滤波器为高斯滤波器,截断为pai=3.14159265357e0)
! data:输入输出-波形数据
! dataE:输出-波形振幅-包络
! dataD:输出-瞬时频率
 implicit none
 integer*4::i,n,logn,nfft
 real*4::data(n),dataE(n),dataIP(n,2)
 real*4::dt,TC,fc,df,f,alpha,fac,freqlw,frequp,G,s
 real*4,parameter::pai=3.14159265357e0
 real*4,allocatable::PR(:),PI(:),FR(:),FI(:),PRD(:),PID(:),FRD(:),FID(:)
 logn=floor(log10(n+0.0e0)/log10(2.0e0))+1
 nfft=2**logn
 fc=1.0e0/TC
 df=1.0e0/(nfft*dt)
 !print*, 'logn=',logn,'nfft=',nfft
 allocate(PR(nfft))
 allocate(PI(nfft))
 allocate(FR(nfft))
 allocate(FI(nfft))
 allocate(PRD(nfft))
 allocate(PID(nfft))
 allocate(FRD(nfft))
 allocate(FID(nfft))
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 PRD=0.0e0
 PID=0.0e0
 FRD=0.0e0
 FID=0.0e0
 PR(1:n)=data(1:n)
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,0)
 fac=sqrt(pai/alpha)
 frequp=(1.0e0+fac)*fc
 freqlw=(1.0e0-fac)*fc
 do i=1,nfft
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
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,1)
 data(1:n)=FR(1:n)
 dataE(1:n)=PR(1:n)
 dataIP(1:n,1)=PI(1:n)*2*pai/360.0e0
 call KKFFT(PRD,PID,nfft,logn,FRD,FID,1,0)
 do i=1,n
  dataIP(i,2)=(FR(i)*FID(i)-FRD(i)*FI(i))/(dataE(i)**2)
  dataIP(i,2)=2*pai/dataIP(i,2)
 enddo
 deallocate(PR)
 deallocate(PI)
 deallocate(FR)
 deallocate(FI)
 deallocate(PRD)
 deallocate(PID)
 deallocate(FRD)
 deallocate(FID)
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
 PR(1)=1.0e0
 PI(1)=0.0e0
 PR(2)=COS(6.283185306e0/N)
 PI(2)=-SIN(6.283185306e0/N)
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
	 PI(I)=ATAN(FI(I)/FR(I))*360.0e0/6.283185306e0
  ENDDO
 ENDIF
 RETURN
END SUBROUTINE
