!功能: 计算两波形数据震相倒时差,用于hypoDD
!注: 两波形数据必须是同一台站记录到的(数据采样间隔一致)
!滤波+按震相对其+滑动窗计算相关系数+再次对其+频域计算时差
!时差为正表示数据2的走时大
!作者: 王清东 时间: 2012-10-19 17:51:25
program ptd
 implicit none

 integer*4::i,j,k,num,nerr
 integer*4::pwln1,pwrn1,pwn1,wln1,wrn1,NPTS1     !pwn:时间窗内的数据点数,最好是2的幂
 integer*4::pwln2,pwrn2,pwn2,wln2,wrn2,NPTS2
 real*4::T1,T2,B1,O1,P1,B2,O2,P2,DT   !保存头文件变量
 real*4::odt,cdt,fdt,pdt,cmax,cc
 real*4::f1,f2,f3,f4,pwl,pwr,slide,tdf1,tdf2
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
 pwl=-0.5e0    !时窗内数据点最好接近2的幂,便于快速傅里叶变换
 pwr=2.0e0
 slide=1.5e0    !时窗滑动量为+-slide
 !震相时差频率域算法中相位最小二乘直线拟合频率范围(Hz)
 tdf1=2.0e0
 tdf2=8.0e0


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
  P1=rhdr(12)
  P1=rhdr(13)
  P1=53.167183+O1
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
  P2=rhdr(9)
  P2=rhdr(12)
  P2=rhdr(13)
  P2=54.815964+O2
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
  call tdf(cor1,pwn1,cor2,pwn2,tdf1,tdf2,DT,fdt)
  odt=(P2-O2)-(P1-O1)  !第一次定位的震相走时差
  cdt=(k-(pwln2-wln2+1))*DT  !移动滑动窗产生的时间差
  pdt=odt+cdt+fdt   !总的时间差

  print*, 'cmax= ',cmax
  print*, 'odt= ',odt
  print*, 'cdt= ',cdt
  print*, 'fdt= ',fdt
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


SUBROUTINE TDF(A,NA,B,NB,F1,F2,DT,FDT)
!频率域计算平方相干值,由互相关相位计算时间差FDT(互相关相位拟合直线斜率)
!A(NA),B(NB):时间序列;DT:采样间隔;F1,F2:相位最小二乘直线拟合频率范围
!CC:互相关最大值;K(2):相位最小二乘直线拟合系数
!CAMP(NFFT),CPHASE(NFFT),MSC(NFFT):互相关谱,互相关相位,平方相干值
 IMPLICIT NONE
 INTEGER*4::I,J,NA,NB,N,LOGN,NFFT
 REAL*4::DT,DF,FDT,F1,F2,RMSEY,RMSE1,RMSE2,PHT,PHF
 REAL*4::A(NA),B(NB),K(2),CC
 REAL*4,ALLOCATABLE::PR(:),PI(:),AR(:),AI(:),BR(:),BI(:)
 REAL*4,ALLOCATABLE::AMP(:),PHASE(:),FREQ(:),X(:),Y(:)
 PHT=1/ABS(F2-F1)  !如果时间差大于PHT,互相关相位会大于2pi而产生跃变
 PHF=1/DT
 PRINT*, 'PHT= ',PHT
 CALL NPOW2(NA+NB-1,NFFT,LOGN)
 ALLOCATE(PR(NFFT),PI(NFFT),AR(NFFT),AI(NFFT),BR(NFFT),BI(NFFT))
 ALLOCATE(AMP(NFFT),PHASE(NFFT),FREQ(NFFT))
 PR=0.0E0
 PI=0.0E0
 AR=0.0E0
 AI=0.0E0
 PR(1:NA)=A(1:NA)
 CALL TAPER(PR,NFFT,0.25)
 CALL KKFFT(PR,PI,NFFT,LOGN,AR,AI,0,0)
 PR=0.0E0
 PI=0.0E0
 BR=0.0E0
 BI=0.0E0
 PR(1:NB)=B(1:NB)
 CALL TAPER(PR,NFFT,0.25)
 CALL KKFFT(PR,PI,NFFT,LOGN,BR,BI,0,0)
 DO I=1,NFFT
  FREQ(I)=(I-1)/(NFFT*DT)
 ENDDO
 PR=AR*BR+AI*BI
 PI=AR*BI-AI*BR
 AMP=SQRT(PR*PR+PI*PI)
 PHASE=ATAN2(PI,PR)  ![-PI,PI]
 !CALL SMOOTH(AMP,NFFT)  !谱平滑
 !CALL SMOOTH(PHASE,NFFT)
 N=0
 DO I=1,NFFT
  IF(FREQ(I).GT.F1.AND.FREQ(I).LT.F2) THEN
   N=N+1
  ENDIF
 ENDDO
 ALLOCATE(X(N),Y(N))
 J=0
 DO I=1,NFFT
  IF(FREQ(I).GT.F1.AND.FREQ(I).LT.F2) THEN
   J=J+1
   X(J)=FREQ(I)
   Y(J)=PHASE(I)
  ENDIF
 ENDDO
 CALL LSLF(X,Y,N,K,CC,RMSEY,RMSE1,RMSE2)
  FDT=-K(2)/6.283185306E0
  !PRINT*, 'KL: ',K(1),K(2)
  !PRINT*, 'RMSEY,RMSE1,RMSE2: ',RMSEY,RMSE1,RMSE2
  PRINT*, 'LSCC= ',CC,'RMSEftd= ',RMSE2/6.283185306E0
 OPEN(11,FILE='FCORF.TXT')
 DO I=1,NFFT
  WRITE(11,*) FREQ(I),AMP(I),PHASE(I),PR(I),PI(I)
 ENDDO
 CLOSE(11)
 OPEN(11,FILE='Line.TXT')
 DO I=1,J
  WRITE(11,*) X(I),Y(I),K(1)+K(2)*X(I)
 ENDDO
 CLOSE(11)
 RETURN
END SUBROUTINE


SUBROUTINE LSLF(X,Y,N,K,CC,RMSEY,RMSE1,RMSE2)
!最小二乘直线拟合
!(X(N),Y(N)):数据坐标,K(2):拟合的直线方程的系数,即Y=K(1)+K(2)*X
!CC:拟合数据与原数据的相关系数;RMSEY:拟合数据与原数据的均方根误差(标准差)
!RMSE1:K(1)的标准差;RMSE2:K(2)的标准差;
 IMPLICIT NONE
 INTEGER*4::N,I,J
 REAL*4::X(N),Y(N),K(2)
 REAL*4::A,B,AA,BB,AB,CC,RMSEY,RMSE1,RMSE2
 A=0.0E0
 B=0.0E0
 AA=0.0E0
 BB=0.0E0
 AB=0.0E0
 DO I=1,N
  A=A+X(I)
  B=B+Y(I)
  AA=AA+X(I)*X(I)
  BB=BB+Y(I)*Y(I)
  AB=AB+X(I)*Y(I)
 ENDDO
 K(2)=(AB*N-A*B)/(AA*N-A*A)
 K(1)=B/N-K(2)*A/N
 CC=(AB-A*B/N)/SQRT((AA-A*A/N)*(BB-B*B/N))
 RMSEY=0.0E0
 DO I=1,N
  RMSEY=RMSEY+(Y(I)-K(1)-K(2)*X(I))*(Y(I)-K(1)-K(2)*X(I))
 ENDDO
 RMSEY=SQRT(RMSEY/(N-2))
 RMSE1=SQRT(AA/(AA*N-A*A))*RMSEY
 RMSE2=RMSEY/SQRT(AA-A*A/N)
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


subroutine smooth(data,n)
!实验数据的等距五点三次平滑
 implicit none
 integer*4::i,n
 real*4::a(n),data(n)
 a(1)=(69*data(1)+4*data(2)-6*data(3)+4*data(4)-data(5))/70
 a(2)=(2*data(1)+27*data(2)+12*data(3)-8*data(4)+2*data(5))/35
 do i=3,n-2
  a(i)=(-3*data(i-2)+12*data(i-1)+17*data(i)+12*data(i+1)-3*data(i+2))/35
 enddo
 a(n-1)=(2*data(n-4)-8*data(n-3)+12*data(n-2)+27*data(n-1)+2*data(n))/35
 a(n)=(-data(n-4)+4*data(n-3)-6*data(n-2)+4*data(n-1)+69*data(n))/70
 data=a
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

