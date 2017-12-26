!功能: 对两个双台(double)互相关SAC文件进行互相关计算(corss-correlation),生成三台(trine)互相关SAC文件
!作者: 王清东 时间: 2011-06-02 11:06:08
program cdt
 implicit none

 integer*4::i,j,k,IRU,nerr
 integer*4::NPTS,NPTS12,NPTS23,NPTS13,NPTSA,NPTSB,N12,N23,N13
 integer*4,parameter::NPTSC=1024              !NPTSC:互相关数据截断(单端长度),一般令NPTSC=2的幂
 real*4::EVLA12,EVLO12,STLA12,STLO12,beign12
 real*4::EVLA23,EVLO23,STLA23,STLO23,beign23
 real*4::EVLA13,EVLO13,STLA13,STLO13,beign13
 real*4::T1,T2             !保存头文件变量
 character*8::KSTNM12,KSTNM23,KSTNM13
 character*60::wname
 character*60::sacname(3)

 real*4,allocatable::data(:),data12(:),data23(:),data13(:)
 real*4,allocatable::d12(:),d23(:),d13(:),dataA(:),dataB(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)

 call CPU_TIME(T1)
 open(10,file='info.txt')                   !程序运行信息文件

!读取控制文件信息
 open(11,file='cdtfile',status='old')       !cdtfile中文件数必为3,且顺序为1-2 2-3 1-3
 do i=1,3
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
 NPTS=NPTSC
 allocate(data(NPTS))
!DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
!STLA=rhdr(32)  STLO=rhdr(33)  STEL=rhdr(34)  STDP=rhdr(35)
!EVLA=rhdr(36)  EVLO=rhdr(37)  EVEL=rhdr(38)  EVDP=rhdr(39)
!NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
!NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)  KSTNM=chdr(1)
!读取1-2双台互相关文件
 call brsach(IRU,sacname(1),nerr)
 beign12=rhdr(6)
 EVLA12=rhdr(36)
 EVLO12=rhdr(37)
 STLA12=rhdr(32)
 STLO12=rhdr(33)
 KSTNM12=chdr(1)
 NPTS12=ihdr(10)
 allocate(data12(NPTS12))
 call brsac(IRU,NPTS12,sacname(1),data12,nerr)
!读取2-3双台互相关文件
 call brsach(IRU,sacname(2),nerr)
 beign23=rhdr(6)
 EVLA23=rhdr(36)
 EVLO23=rhdr(37)
 STLA23=rhdr(32)
 STLO23=rhdr(33)
 KSTNM23=chdr(1)
 NPTS23=ihdr(10)
 allocate(data23(NPTS23))
 call brsac(IRU,NPTS23,sacname(2),data23,nerr)
!读取1-3双台互相关文件
 call brsach(IRU,sacname(3),nerr)
 beign13=rhdr(6)
 EVLA13=rhdr(36)
 EVLO13=rhdr(37)
 STLA13=rhdr(32)
 STLO13=rhdr(33)
 KSTNM13=chdr(1)
 NPTS13=ihdr(10)
 allocate(data13(NPTS13))
 call brsac(IRU,NPTS13,sacname(3),data13,nerr)

!计算非因果信号的三台互相关序列
 N12=anint(-beign12/rhdr(1))+1
 N23=anint(-beign23/rhdr(1))+1
 N13=anint(-beign13/rhdr(1))+1
 if(NPTSC.gt.N12.or.NPTSC.gt.N23.or.NPTSC.gt.N13) then
  print*, 'The NPTSC is too large, please check!'
  goto 99
 endif
 allocate(d12(N12))
 do i=1,N12
  d12(i)=data12(N12-i+1)
 enddo
 allocate(d23(N23))
 do i=1,N23
  d23(i)=data23(N23-i+1)
 enddo
 allocate(d13(N13))
 do i=1,N13
  d13(i)=data13(N13-i+1)
 enddo
 NPTSA=N12+N13-1
 NPTSB=N23+N13-1
!计算1-2与1-3的互相关序列
 allocate(dataA(NPTSA))
 call SCOR(d12,N12,d13,N13,dataA)
 do i=1,NPTS
  data(i)=dataA(N12+i-1)
 enddo
!生成1-2与1-3的互相关得到的三台文件
 rhdr(36)=STLA12
 rhdr(37)=STLO12
 rhdr(32)=STLA13
 rhdr(33)=STLO13
 rhdr(6)=beign13-beign12
 rhdr(7)=rhdr(6)+(NPTS-1)*rhdr(1)
 chdr(1)=KSTNM12(5:8)//KSTNM13(5:8)
 wname(1:16)=KSTNM12(5:8)//'.'//KSTNM13(5:8)//'.3-.SAC'
 wname=adjustl(wname)
 print*, trim(wname)
 call wsac(data,NPTS,wname)
!计算2-3与1-3的互相关序列
 allocate(dataB(NPTSB))
 call SCOR(d23,N23,d13,N13,dataB)
 do i=1,NPTS
  data(i)=dataB(N23+i-1)
 enddo
!生成2-3与1-3的互相关得到的三台文件
 rhdr(36)=EVLA13
 rhdr(37)=EVLO13
 rhdr(32)=EVLA23
 rhdr(33)=EVLO23
 rhdr(6)=beign13-beign23
 rhdr(7)=rhdr(6)+(NPTS-1)*rhdr(1)
 chdr(1)=KSTNM13(1:4)//KSTNM23(1:4)
 wname(1:16)=KSTNM13(1:4)//'.'//KSTNM23(1:4)//'.3-.SAC'
 wname=adjustl(wname)
 print*, trim(wname)
 call wsac(data,NPTS,wname)
 deallocate(dataB,dataA,d12,d23,d13)

!计算因果信号的三台互相关序列
 N12=NPTS12-N12+1
 N23=NPTS23-N23+1
 N13=NPTS13-N13+1
 if(NPTSC.gt.N12.or.NPTSC.gt.N23.or.NPTSC.gt.N13) then
  print*, 'The NPTSC is too large, please check!'
  goto 99
 endif
 allocate(d12(N12))
 do i=1,N12
  d12(i)=data12(NPTS12-N12+i)
 enddo
 allocate(d23(N23))
 do i=1,N23
  d23(i)=data23(NPTS23-N23+i)
 enddo
 allocate(d13(N13))
 do i=1,N13
  d13(i)=data13(NPTS13-N13+i)
 enddo
 NPTSA=N12+N13-1
 NPTSB=N23+N13-1
!计算1-2与1-3的互相关序列
 allocate(dataA(NPTSA))
 call SCOR(d12,N12,d13,N13,dataA)
 do i=1,NPTS
  data(i)=dataA(N12+i-1)
 enddo
!生成1-2与1-3的互相关得到的三台文件
 rhdr(36)=STLA12
 rhdr(37)=STLO12
 rhdr(32)=STLA13
 rhdr(33)=STLO13
 rhdr(6)=beign13-beign12
 rhdr(7)=rhdr(6)+(NPTS-1)*rhdr(1)
 chdr(1)=KSTNM12(5:8)//KSTNM13(5:8)
 wname(1:16)=KSTNM12(5:8)//'.'//KSTNM13(5:8)//'.3+.SAC'
 wname=adjustl(wname)
 print*, trim(wname)
 call wsac(data,NPTS,wname)
!计算2-3与1-3的互相关序列
 allocate(dataB(NPTSB))
 call SCOR(d23,N23,d13,N13,dataB)
 do i=1,NPTS
  data(i)=dataB(N23+i-1)
 enddo
!生成2-3与1-3的互相关得到的三台文件
 rhdr(36)=EVLA13
 rhdr(37)=EVLO13
 rhdr(32)=EVLA23
 rhdr(33)=EVLO23
 rhdr(6)=beign13-beign23
 rhdr(7)=rhdr(6)+(NPTS-1)*rhdr(1)
 chdr(1)=KSTNM13(1:4)//KSTNM23(1:4)
 wname(1:16)=KSTNM13(1:4)//'.'//KSTNM23(1:4)//'.3+.SAC'
 wname=adjustl(wname)
 print*, trim(wname)
 call wsac(data,NPTS,wname)
 deallocate(dataB,dataA,d12,d23,d13)

 99 continue
 deallocate(data13,data23,data12,data)
 close(10)
 call CPU_TIME(T2)
 print*, 'T=',T2-T1
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


SUBROUTINE SCOR(X,M,H,N,Y)
 IMPLICIT NONE
! THIS PROGRAM IS PURPOSED TO CALCULATE THE AUTO-CORRELATION OR
! CORSS-CORRELATION FUNCTION IN FREQUENCE-DOMAIN(FORM SAC2000). CREATIED: 2011-01-04 20:20:31
 INTEGER*4::I,J,M,N,LOGN,NFFT
 REAL*4::X(M),H(N),Y(M+N-1),XR,XI,YR,YI
 REAL*4,ALLOCATABLE::PR(:),PI(:),FR(:),FI(:)
! X(M),H(N)为输入，Y(M+N-1)为输出；Y为X与H的相关序列，共M+N-1项
 CALL NPOW2(M+N-1,NFFT,LOGN)
 ALLOCATE(PR(NFFT),PI(NFFT),FR(NFFT),FI(NFFT))
 PR=0.0E0
 PI=0.0E0
 FR=0.0E0
 FI=0.0E0
 CALL TAPER(X,M,0.005)
 CALL TAPER(H,N,0.005)
 PR(1:M)=X(1:M)
 PI(1:N)=H(1:N)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,0,0)
 PR=0.0E0
 PI=0.0E0
 PR(1)=FR(1)*FI(1)
 DO I=2,NFFT/2+1
  J=NFFT-I+2
  XR=(FR(I)+FR(J))/2
	XI=(FI(I)-FI(J))/2
	YR=(FI(I)+FI(J))/2
	YI=(FR(J)-FR(I))/2
	PR(I)=XR*YR+XI*YI
  PI(I)=XR*YI-XI*YR
	PR(J)=PR(I)
	PI(J)=-PI(I)
 ENDDO
 CALL TAPER(PR,NFFT,0.005)
 CALL TAPER(PI,NFFT,0.005)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,1,0)
! Y(1:NLEN1-1)=FR(NFFT-NLEN1+2:NFFT)
! Y(NLEN1:NLEN1+NLEN2-1)=FR(1:NLEN2)
 DO I=1,M-1
  Y(I)=FR(NFFT-M+I+1)
 ENDDO
 DO I=1,N
  Y(M+I-1)=FR(I)
 ENDDO
 DEALLOCATE(PR,PI,FR,FI)
 RETURN
END SUBROUTINE


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
   IF(FR(I).LT.0.0E0) PI(I)=180.0E0+PI(I)
   IF(FI(I).LE.0E0.AND.FR(I).GE.0.0E0) PI(I)=360.0E0+PI(I)
  ENDDO
 ENDIF
 RETURN
END SUBROUTINE


subroutine wsac(data,n,wname)
 implicit none
 integer*4::i,n
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
 call bwsac(20,ihdr(10),wname,data)
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
