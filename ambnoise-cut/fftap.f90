!以周期-模返回傅里叶变换谱（搜索法）
program fftap
 implicit none

 integer*4::i,j,k,num,ntc,nfft,logn,IRU,NPTS,nerr
 real*4::btc,etc,dtc,dt,df
 real*4,allocatable::tc(:),PR(:),PI(:),FR(:),FI(:),f(:),ff(:),temp2(:)
 character*60::wname
 character*60,allocatable::sacname(:)

 real*4,allocatable::data(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)

!读取控制文件信息
 open(11,file='fftapfile',status='old')
 read(11,*) num,btc,etc,dtc
 allocate(sacname(num))
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

 ntc=anint(abs((etc-btc)/dtc))+1
 allocate(tc(ntc),temp2(ntc))
 do i=1,ntc
  tc(i)=btc+(i-1)*dtc
 enddo

 IRU=100
 do i=1,num
  call brsach(IRU,sacname(i),nerr)
  NPTS=ihdr(10)
  dt=rhdr(1)
  call npow2(NPTS,nfft,logn)
  !print*, NPTS,nfft
  allocate(data(NPTS),PR(nfft),PI(nfft),FR(nfft),FI(nfft))
  allocate(f(nfft/2+1),ff(nfft/2+1))
  call brsac(IRU,NPTS,sacname(i),data,nerr)
  PR=0.0e0
  PI=0.0e0
  FR=0.0e0
  FI=0.0e0
  !call taper(data,NPTS,0.05)
  PR(1:NPTS)=data(1:NPTS)*sqrt(rhdr(51))
  call KKFFT(PR,PI,nfft,logn,FR,FI,0,1)
  df=1/(nfft*dt)
  do j=1,nfft/2+1
   f(j)=(j-1)*df
  enddo
  do j=1,ntc
   ff=abs(f-1/tc(j))
   k=minloc(ff,1)
   temp2(j)=PR(k)
  enddo
  !call nor(temp2,ntc)
  wname=trim(sacname(i))//'.ap'
  open(IRU,file=wname)
  print*, trim(wname)
  do j=1,ntc
   write(IRU,*) tc(j),temp2(j)
  enddo
  close(IRU)
  deallocate(data,PR,PI,FR,FI,f,ff)
 enddo

 deallocate(tc,temp2)
 99 continue
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
 if(max.eq.0.0e0) then
  print*, 'This is a zero data, please check'
  return
 endif
 data=data/max
 return
end subroutine
