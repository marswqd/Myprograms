!功能: 计算两波形数据震相倒时差,用于hypoDD
!注: 两波形数据必须是同一台站记录到的(数据采样间隔一致)
!滤波+按震相对其+滑动窗计算相关系数+再次对其+时域计算时差
!时差为正表示数据2的走时大
!作者: 王清东 时间: 2012-10-19 17:51:29
program ptd
 implicit none

 integer*4::i,j,k,num,nerr
 integer*4::pwln1,pwrn1,pwn1,wln1,wrn1,NPTS1     !pwn:时间窗内的数据点数,最好是2的幂
 integer*4::pwln2,pwrn2,pwn2,wln2,wrn2,NPTS2
 real*4::T1,T2,B1,O1,P1,B2,O2,P2,DT   !保存头文件变量
 real*4::odt,cdt,tdt,pdt,cmax,cc
 real*4::f1,f2,f3,f4,pwl,pwr,slide
 character*60,allocatable::sacname(:)

 real*4,allocatable::data1(:),data2(:),cor1(:),cor2(:),cor(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)


!读取控制文件信息
 open(11,file='ptd.in',status='old')
 read(11,*) num
 allocate(sacname(num*2))
 do i=1,num*2
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

!参数设定
 !带通滤波参数(Hz)
 f1=1.0e0
 f2=2.0e0
 f3=8.0e0
 f4=10.e0
 !时窗设定,震相到时左右偏移量(s)
 pwl=-1.0e0    !时窗内数据点最好接近2的幂,便于快速傅里叶变换
 pwr=1.0e0
 slide=1.5e0    !时窗滑动量为+-slide


! DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
! A=rhdr(9)      T0=rhdr(11)    T1=rhdr(12)    T2=rhdr(13)
! STLA=rhdr(32)  STLO=rhdr(33)  STEL=rhdr(34)  STDP=rhdr(35)
! EVLA=rhdr(36)  EVLO=rhdr(37)  EVEL=rhdr(38)  EVDP=rhdr(39)
! NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
! NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)  KSTNM=chdr(1)
 do i=1,num*2-1,2
!读取互相关计算中的主文件(固定窗)
  call brsach(100,sacname(i),nerr)
  NPTS1=ihdr(10)
  DT=rhdr(1)  !采样间隔
  B1=rhdr(6)  !第一个采样点的时刻
  O1=rhdr(8)  !发震时刻
  P1=rhdr(9)  !震相到时,一般为P波到时
  !P1=rhdr(12)
  !P1=rhdr(13)
  !P1=53.167183+O1
  print*, 'P1= ',P1
  wln1=anint((P1+pwl-slide-B1)/DT+1)  !判断边界是否超出数据范围
  wrn1=anint((P1+pwr+slide-B1)/DT+1)
  if(wrn1.gt.NPTS1.or.wln1.lt.1) then
   print*, 'The window is larger than the data of ',trim(sacname(i)),', please check!'
   goto 99
  endif
  pwln1=anint((P1+pwl-B1)/DT+1)  !确定固定窗
  pwrn1=anint((P1+pwr-B1)/DT+1)
  pwn1=abs(pwrn1-pwln1+1)
  !print*, 'PWN1= ',pwn1
  allocate(data1(NPTS1),cor1(pwn1))
  call brsac(100,NPTS1,sacname(i),data1,nerr)
  !call nor(data1,NPTS1)
  !call bpfn(data1,NPTS1,DT,1/f4,1/f3,1/f2,1/f1)  !带通滤波
  cor1(1:pwn1)=data1(pwln1:pwrn1)
!读取互相关计算中的副文件(滑动窗)
  call brsach(100,sacname(i+1),nerr)
  NPTS2=ihdr(10)
  DT=rhdr(1)
  B2=rhdr(6)
  O2=rhdr(8)
  P2=rhdr(9)+2.1
  !P2=rhdr(12)+2.5
  !P2=rhdr(13)
  !P2=54.815964+O2
  print*, 'P2= ',P2
  wln2=anint((P2+pwl-slide-B2)/DT+1)  !判断边界是否超出数据范围
  wrn2=anint((P2+pwr+slide-B2)/DT+1)
  if(wrn2.gt.NPTS2.or.wln2.lt.1) then
   print*, 'The window is larger than the data of ',trim(sacname(i+1)),', please check!'
   goto 99
  endif
  pwln2=anint((P2+pwl-B2)/DT+1)
  pwrn2=anint((P2+pwr-B2)/DT+1)
  pwn2=abs(pwrn2-pwln2+1)
  !print*, 'PWN2= ',pwn2
  if(pwn1.ne.pwn2) then
   print*, 'The length of the two phase windows are not the same, please check!'
   goto 99
  endif
  print*, 'PWN= ',pwn2
  wrn2=anint((P2+pwl+slide-B2)/DT+1)
  allocate(data2(NPTS2),cor2(pwn2),cor(wrn2-wln2+1))
  !print*, 'wrn2-wln2+1= ',wrn2-wln2+1
  call brsac(100,NPTS2,sacname(i+1),data2,nerr)
  !call nor(data2,NPTS2)
  !call bpfn(data2,NPTS2,DT,1/f4,1/f3,1/f2,1/f1)  !带通滤波
  cmax=-99999.0e0
  !call nor(cor1,pwn1)
  do j=1,wrn2-wln2+1                           !滑动起来
   cor2(1:pwn2)=data2(wln2+j-1:wln2+pwn2-1+j-1)
   !call nor(cor2,pwn2)
   call corcoe(cor1,cor2,pwn1,cc)       !计算相关系数
   cor(j)=cc
   if(cmax.lt.cc) then
    cmax=cc
    k=j
   endif
  enddo

  open(11,file='COR.TXT')
  do j=1,wrn2-wln2+1
   write(11,*) (j-(pwln2-wln2+1))*DT,cor(j)
  enddo
  close(11)

  cor2(1:pwn2)=data2(wln2+k-1:wln2+pwn2-1+k-1) !移动滑动窗,使两者相关系数最大
  !call nor(cor2,pwn2)
  open(11,file='DATA.TXT')
  do j=1,pwn1
   write(11,*) cor1(j),cor2(j)
  enddo
  close(11)
  print*, ' '
  call td(cor1,pwn1,cor2,pwn2,DT,tdt)
  odt=(P2-O2)-(P1-O1)  !第一次定位的震相走时差
  cdt=(k-(pwln2-wln2+1))*DT  !移动滑动窗产生的时间差
  pdt=odt+cdt   !总的时间差

  print*, 'cmax= ',cmax
  print*, 'odt= ',odt
  print*, 'cdt= ',cdt
  if(tdt.ne.0.0e0) then
   print*, 'tdt= ',tdt
   print*, 'The slide momentum may be too small, please check!'
  endif
  print*, 'pdt= ',pdt

  deallocate(data1,data2,cor1,cor2,cor)
 enddo

 99 continue
 deallocate(sacname)
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


subroutine bpfn(data,n,dt,t1,t2,t3,t4)
 implicit none
 integer::i,n,logn,nfft
 real*4::dt,df,t1,t2,t3,t4,f1,f2,f3,f4,f,bp
 real*4::data(n)
 real*4,allocatable::PR(:),PI(:),FR(:),FI(:)
 real*4,parameter::pai=3.14159265358e0
 call npow2(n,nfft,logn)
 allocate(PR(nfft),PI(nfft),FR(nfft),FI(nfft))
 df=1.0e0/(nfft*dt)
 f1=1.0e0/t4
 f2=1.0e0/t3
 f3=1.0e0/t2
 f4=1.0e0/t1
 !print*, df,f1,f2,f3,f4
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 PR(1:n)=data(1:n)
 call taper(PR,nfft,0.25)                !平滑数据的两端,避免吉普斯现象
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,0)
 do i=1,nfft/2+1
  f=(i-1)*df
  if(f.ge.f1.and.f.le.f2) then
   bp=0.5e0*(cos(pai*(f-f2)/(f2-f1))+1.0e0)
  elseif(f.gt.f2.and.f.lt.f3) then
   bp=1.0e0
  elseif(f.ge.f3.and.f.le.f4) then
   bp=0.5e0*(cos(pai*(f-f3)/(f4-f3))+1.0e0)
  else
   bp=0.0e0
  endif
  PR(i)=FR(i)*bp
  PI(i)=FI(i)*bp
 enddo
 PR(1)=PR(1)/2
 PI(1)=PI(1)/2
 PR(nfft/2+1)=PR(nfft/2+1)/2
 PI(nfft/2+1)=PI(nfft/2+1)/2
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !令负频率为零
 !call taper(PR,nfft,0.005)
 !call taper(PI,nfft,0.005)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 data(1:n)=2*FR(1:n)
 return
end subroutine


subroutine corcoe(x,y,n,cc)
!本程序计算x(n),y(n)两列向量的相关系数cc
 implicit none
 integer*4::n,i
 real*4::x(n),y(n),cc,a,b,aa,bb,ab
 a=0.0e0
 b=0.0e0
 aa=0.0e0
 bb=0.0e0
 ab=0.0e0
 do i=1,n
  a=a+x(i)
  b=b+y(i)
  aa=aa+x(i)*x(i)
  bb=bb+y(i)*y(i)
  ab=ab+x(i)*y(i)
 enddo
 cc=(ab-a*b/n)/sqrt((aa-a*a/n)*(bb-b*b/n))
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
 if(i.eq.n) then
  print*, 'This is a zero data, please check'
  return
 endif
 tl=i
 do i=n,1,-1
  if(data(i).ne.0.0e0) exit
 enddo
 tr=i
 f0=0.5e0
 f1=0.5e0
 j=anint((tr-tl+1)*width)
 if(j.eq.1.or.j.eq.0) return
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


SUBROUTINE TD(A,NA,B,NB,DT,TDT)
!时间域计算互相关函数,得到时间差TDT(互相关最大值对应时间偏移)
!A(NA),B(NB):时间序列;DT:采样间隔;CC:互相关最大值;
 IMPLICIT NONE
 INTEGER*4::I,J,NA,NB
 REAL*4::DT,TDT,AA,BB,CC
 REAL*4::A(NA),B(NB),C(-NA+1:NB-1)
!时域算法
 !CALL NOR(A,NA)
 !CALL NOR(B,NB)
 CALL TCOR(A,NA,B,NB,C,-NA+1,NB-1)  !时域中计算互相关函数
 AA=0.0E0
 BB=0.0E0
 !PRINT*, 'A(1)= ',A(1)
 !PRINT*, 'B(1)= ',B(1)
 DO I=1,NA
  AA=AA+A(I)*A(I)
 ENDDO
 DO I=1,NB
  BB=BB+B(I)*B(I)
 ENDDO
 C=C/SQRT(AA*BB)
 CC=0.0E0
 J=0
 DO I=-NA+1,NB-1
  IF(CC.LT.C(I)) THEN
   CC=C(I)
   J=I
  ENDIF
 ENDDO
 PRINT*, 'J= ',J,'CF= ',CC
 TDT=J*DT
 OPEN(11,FILE='TCOR.TXT')
 DO I=-NA+1,NB-1
  WRITE(11,*) I*DT,C(I)
 ENDDO
 CLOSE(11)
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


SUBROUTINE TCOR(X,M,H,N,Y,LN,RN)
 IMPLICIT NONE
! This program is purposed to calculate the auto-correlation or
! corss-correlation function in time-domain. Creatied: 2010/12/17 15:25:44
 INTEGER*4::I,J,M,N,LN,RN
 REAL*4::X(M),H(N),Y(LN:RN)
! X(M),H(N)为输入，Y(-M+1:N-1)为输出；Y为X与H的相关序列，共M+N-1项
! Y(0)  为X(M),H(N)首项对齐时的相关值；
! Y(1)  为H(N)相对X(M)左移一项时的相关值，正向分支共N-1项；
! Y(-1) 为H(N)相对X(M)右移一项时的相关值，负向分支共M-1项。
! LN表示所求的相关序列的左边界，RN表示所求的相关序列的右边界。
 IF(LN.LT.-M+1) PRINT*, '左边界超出'
 IF(RN.GT.N-1) PRINT*, '右边界超出'
 DO I=LN,RN
  Y(I)=0.0e0
  DO J=1,M
   IF(J+I.GE.1.AND.J+I.LE.N) Y(I)=Y(I)+X(J)*H(J+I)
  ENDDO
 ENDDO
 RETURN
END SUBROUTINE


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

