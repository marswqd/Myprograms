!功能: 计算两波形数据震相倒时差，用于hypoDD
!注: 两波形数据必须是同一台站记录到的
!作者: 王清东 时间: 2012-10-01 14:45:51
!未完成:
!截取数据(+-5s)+滤波+插值+计算相关系数+时频域计算时差
program ptd
 implicit none

 integer*4::i,j,k,NPTS,num,nfft,logn,nerr
 integer*4::pwln,pwrn,pwn     !pwn:时间窗内的数据点数,最好是2的幂
 real*4,parameter::pwl=-0.5e0,pwr=2.0e0
 real*4::B,A,T0,DT,f1,f2,FDT,cc,DDT,T1,T2             !保存头文件变量
 character*60,allocatable::sacname(:)

 real*4,allocatable::data(:),temp(:),dataA(:),dataB(:)
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
 !带通滤波参数
 f1=1.0e0
 f2=2.0e0
 f3=8.0e0
 f4=10.e0
 !插值序列时间间隔 100Hz
 inDT=0.01e0
 !时窗设定，震相到时左右偏移量
 pwl=-0.56e0    !时窗内数据点数为256(或2的幂),便于快速傅里叶变换
 pwr=2.0e0
 slide=1.5e0    !时窗滑动量为+-slide
 !震相时差频率域算法中相位最小二乘直线拟合频率范围
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
  NPTS=ihdr(10)
  DT=rhdr(1)
  B=rhdr(6)
  A=rhdr(9)  !震相到时,一般为P波到时
  !A=rhdr(12)
  print*, 'A= ',A
  wln=anint((A-5.0e0-B)/DT+1)  !截取震相到时点前后5s的数据,插值使之采样率为100Hz
  wrn=anint((A+5.0e0-B)/DT+1)
  if(pwrn.gt.NPTS.or.wln.lt.1) then
   print*, 'The window is larger than the data of ',trim(sacname(i)),', please check!'
   goto 99
  endif
  wn=abs(wrn-wln+1)
  inN=(wn-1)*DT/inDT+1
  print*, 'WN= ',wn
  allocate(data(NPTS),tt(wn),temp(wn),tt1(inN),temp1(inN))
  call brsac(100,NPTS,sacname(i),data,nerr)
  call bpfn(data,NPTS,DT,1/f4,1/f3,1/f2,1/f1)  !带通滤波
  temp(1:wn)=data(wln:wrn)
  do j=1,wn
   tt(j)=B+(wln+j-2)*DT
  enddo
  do j=1,inN
   tt1(j)=tt(1)+(j-1)*inDT
  enddo
  dy1=(temp(2)-temp(1))/(tt(2)-tt(1))
  dyn=(temp(wn)-temp(wn-1))/(tt(wn)-tt(wn-1))
  !print*, dy1,dyn
  temp1=0.0e0
  call espl1(tt,temp,wn,dy1,dyn,tt1,inN,temp1)
  pwln=anint((A+pwl-tt(1))/inDT+1)
  pwrn=anint((A+pwr-tt(1))/inDT+1)
  pwn=abs(pwrn-pwln+1)
  print*, 'PWN= ',pwn
  allocate(dataA(pwn))
  dataA(1:pwn)=temp1(pwln:pwrn)
  deallocate(data,tt,temp,tt1,temp1)
!读取互相关计算中的副文件(滑动窗)
  call brsach(100,sacname(i+1),nerr)
  NPTS=ihdr(10)
  DT=rhdr(1)
  B=rhdr(6)
  A=rhdr(9)  !震相到时,一般为P波到时
  !A=rhdr(12)
  print*, 'A= ',A
  wln=anint((A-5.0e0-B)/DT+1)  !截取震相到时点前后5s的数据,插值使之采样率为100Hz
  wrn=anint((A+5.0e0-B)/DT+1)
  if(pwrn.gt.NPTS.or.wln.lt.1) then
   print*, 'The window is larger than the data of ',trim(sacname(i+1)),', please check!'
   goto 99
  endif
  wn=abs(wrn-wln+1)
  inN=(wn-1)*DT/inDT+1
  print*, 'WN= ',wn
  allocate(data(NPTS),tt(wn),temp(wn),tt1(inN),temp1(inN))
  call brsac(100,NPTS,sacname(i+1),data,nerr)
  call bpfn(data,NPTS,DT,1/f4,1/f3,1/f2,1/f1)  !带通滤波
  temp(1:wn)=data(wln:wrn)
  do j=1,wn
   tt(j)=B+(wln+j-2)*DT
  enddo
  do j=1,inN
   tt1(j)=tt(1)+(j-1)*inDT
  enddo
  dy1=(temp(2)-temp(1))/(tt(2)-tt(1))
  dyn=(temp(wn)-temp(wn-1))/(tt(wn)-tt(wn-1))
  !print*, dy1,dyn
  temp1=0.0e0
  call espl1(tt,temp,wn,dy1,dyn,tt1,inN,temp1)
  pwln=anint((A+pwl-tt(1))/inDT+1)
  pwrn=anint((A+pwr-tt(1))/inDT+1)
  pwn=abs(pwrn-pwln+1)
  print*, 'PWN= ',pwn
  allocate(dataA(pwn))
  wln=anint((A+pwl-slide-tt(1))/inDT+1)  !确定滑动窗的范围
  wrn=anint((A+pwl+slide-tt(1))/inDT+1)
  allocate(cor(wrn-wln+1))
  cmax=-2.0e0
  do j=wln,wrn                           !滑动起来
   dataB(1:pwn)=temp1(j:pwn+j-1)
   call corcoe(dataA,dataB,pwn,cc)       !计算相关系数
   cor(j-wln+1)=cc
   if(cmax.lt.cc) then
    cmax=cc
    k=j
  enddo
  deallocate(data,tt,temp,tt1,temp1)


!频域算法
  !call npow2(pwn,nfft,logn)
  call nor(dataA,pwn)
  call nor(dataB,pwn)
  OPEN(11,FILE='AB.TXT')
  DO j=1,pwn
   WRITE(11,*) dataA(j),dataB(j)
  ENDDO
 CLOSE(11)
  call tdf(dataA,pwn,dataB,pwn,f1,f2,DT,fdt)
!时域算法
  call tdt(dataA,pwn,dataB,pwn,DT,cc,ddt)
  print*, 'fdt= ',fdt
  print*, 'ddt= ',ddt,'cc= ',cc

 enddo

 99 continue
 deallocate(sacname)
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


subroutine bpfn(data,n,dt,t1,t2,t3,t4)
 implicit none
 integer::i,n,logn,nfft
 real*4::dt,df,t1,t2,t3,t4,f1,f2,f3,f4,f,bp
 real*4::data(n)
 real*4,allocatable::PR(:),PI(:),FR(:),FI(:)
 real*4,parameter::pai=3.14159265358e0
 call npw2(n,nfft,logn)
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
 call taper(PR,nfft,0.05)                !平滑数据的两端,避免吉普斯现象
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


subroutine espl1(x,y,n,dy1,dyn,xx,m,s)
!subroutine espl1(x,y,n,dy1,dyn,xx,m,dy,ddy,s,ds,dds,t,h)
!给定端点一阶导数的三次样条插值-徐士良
!x(n),y(n): 输入参数，n个结点值和函数值
!dy1,dyn: 输入参数，第一个结点和最后一个结点的一阶导数值
!xx(m): 输入参数，m个插值点值
!dy(n),ddy(n): 输出参数，n个给定结点处的一阶导数值和二阶导数值
!s(m),ds(m),dds(m):输出参数，m个指定插值点处函数值、一阶导数值和二阶导数值
!t: 输出参数，插值区间[x1,xn]间上的积分值
!h(n): 工作数组
 implicit none
 integer*4::n,m,j,i
 real*4::dy1,dyn,t,h0,h1,beta,alpha
 real*4::x(n),y(n),xx(m),dy(n),ddy(n),s(m),ds(m),dds(m),h(n)
 dy(1)=0.0e0
 h(1)=dy1
 h0=x(2)-x(1)
 do j=2,n-1
  h1=x(j+1)-x(j)
  alpha=h0/(h0+h1)
  beta=(1.0e0-alpha)*(y(j)-y(j-1))/h0
  beta=3.0e0*(beta+alpha*(y(j+1)-y(j))/h1)
  dy(j)=-alpha/(2.0e0+(1.0e0-alpha)*dy(j-1))
  h(j)=(beta-(1.0e0-alpha)*h(j-1))
  h(j)=h(j)/(2.0e0+(1.0e0-alpha)*dy(j-1))
  h0=h1
 enddo
 dy(n)=dyn
 do j=n-1,1,-1
  dy(j)=dy(j)*dy(j+1)+h(j)
 enddo
 do j=1,n-1
  h(j)=x(j+1)-x(j)
 enddo
 do j=1,n-1
  h1=h(j)*h(j)
  ddy(j)=6.0e0*(y(j+1)-y(j))/h1-2.0e0*(2.0e0*dy(j)+dy(j+1))/h(j)
 enddo
 h1=h(n-1)*h(n-1)
 ddy(n)=6.0e0*(y(n-1)-y(n))/h1+2.0e0*(2.0e0*dy(n)+dy(n-1))/h(n-1)
 t=0.0e0
 do i=1,n-1
  h1=0.5e0*h(i)*(y(i)+y(i+1))
  h1=h1-h(i)*h(i)*h(i)*(ddy(i)+ddy(i+1))/24.0e0
  t=t+h1
 enddo
 do 70 j=1,m
  if(xx(j).ge.x(n)) then
   i=n-1
  else
   i=1
 60 if (xx(j).gt.x(i+1)) then
    i=i+1
    goto 60
   endif
  endif
  h1=(x(i+1)-xx(j))/h(i)
  s(j)=(3.0e0*h1*h1-2.0e0*h1*h1*h1)*y(i)
  s(j)=s(j)+h(i)*(h1*h1-h1*h1*h1)*dy(i)
  ds(j)=6.0e0*(h1*h1-h1)*y(i)/h(i)
  ds(j)=ds(j)+(3.0e0*h1*h1-2.0e0*h1)*dy(i)
  dds(j)=(6.0e0-12.0e0*h1)*y(i)/(h(i)*h(i))
  dds(j)=dds(j)+(2.0e0-6.0e0*h1)*dy(i)/h(i)
  h1=(xx(j)-x(i))/h(i)
  s(j)=s(j)+(3.0e0*h1*h1-2.0e0*h1*h1*h1)*y(i+1)
  s(j)=s(j)-h(i)*(h1*h1-h1*h1*h1)*dy(i+1)
  ds(j)=ds(j)-6.0e0*(h1*h1-h1)*y(i+1)/h(i)
  ds(j)=ds(j)+(3.0e0*h1*h1-2.0e0*h1)*dy(i+1)
  dds(j)=dds(j)+(6.0e0-12.0e0*h1)*y(i+1)/(h(i)*h(i))
  dds(j)=dds(j)-(2.0e0-6.0e0*h1)*dy(i+1)/h(i)
 70 continue
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
!频率域计算平方相干值,由互相关谱计算时间差
!A(NA),B(NB):时间序列;DT:采样间隔
!F1,F2:相位最小二乘直线拟合频率范围
!FDT:时间差(互相关相位拟合直线的斜率)
!AMP(NFFT),PHASE(NFFT),MSC(NFFT):互相关谱,互相关相位,平方相干值
 IMPLICIT NONE
 INTEGER*4::I,J,NA,NB,N,LOGN,NFFT
 REAL*4::DT,DF,FDT,A(NA),B(NB),C(2),F1,F2,AA,BB
 REAL*4,ALLOCATABLE::PR(:),PI(:),AR(:),AI(:),BR(:),BI(:),AMP(:),PHASE(:),MSC(:),FREQ(:),X(:),Y(:)
 CALL NPOW2(NA+NB-1,NFFT,LOGN)
 ALLOCATE(PR(NFFT),PI(NFFT),AR(NFFT),AI(NFFT),BR(NFFT),BI(NFFT))
 ALLOCATE(AMP(NFFT),PHASE(NFFT),MSC(NFFT),FREQ(NFFT))
 PR=0.0E0
 PI=0.0E0
 AR=0.0E0
 AI=0.0E0
 PR(1:NA)=A(1:NA)
 CALL TAPER(PR,NFFT,0.5)
 AA=0.0E0
 DO I=1,NFFT
  AA=AA+PR(I)*PR(I)
 ENDDO
 CALL KKFFT(PR,PI,NFFT,LOGN,AR,AI,0,0)
 PR=0.0E0
 PI=0.0E0
 BR=0.0E0
 BI=0.0E0
 PR(1:NB)=B(1:NB)
 CALL TAPER(PR,NFFT,0.5)
 BB=0.0E0
 DO I=1,NFFT
  BB=BB+PR(I)*PR(I)
 ENDDO
 CALL KKFFT(PR,PI,NFFT,LOGN,BR,BI,0,0)
 DO I=1,NFFT
  PR(I)=AR(I)*BR(I)+AI(I)*BI(I)
  PI(I)=AR(I)*BI(I)-AI(I)*BR(I)
  AMP(I)=SQRT(PR(I)*PR(I)+PI(I)*PI(I))
  PHASE(I)=ATAN2(PI(I),PR(I))  ![-PI,PI]
  MSC(I)=(AMP(I)*AMP(I))/(SQRT(AR(I)*AR(I)+AI(I)*AI(I))*SQRT(BR(I)*BR(I)+BI(I)*BI(I)))
  FREQ(I)=(I-1)/(NFFT*DT)
 ENDDO
 MSC=MSC/SQRT(AA*BB)
 J=0
 N=0
 DO I=1,NFFT
  IF(FREQ(I).GT.F1.AND.FREQ(I).LT.F2) THEN
   N=N+1
   !IF(MSC(I).GT.0.9E0) J=J+1
  ENDIF
 ENDDO
 !IF(J*1.0E0/N.GT.0.5E0) THEN
  ALLOCATE(X(N),Y(N))
  J=0
  DO I=1,NFFT
   IF(FREQ(I).GT.F1.AND.FREQ(I).LT.F2) THEN
    J=J+1
    X(J)=FREQ(I)
    Y(J)=PHASE(I)!*MSC(I)*MSC(I)/(1-MSC(I)*MSC(I))
   ENDIF
  ENDDO
  CALL HPIR1(X,Y,N,C,2,DF,DF,DF)
  FDT=C(2)/6.283185306E0
  PRINT*, 'C: ',C(1),C(2)
 !ENDIF

 OPEN(11,FILE='MSC.TXT')
 DO I=2,NFFT
  WRITE(11,*) FREQ(I),AMP(I),PHASE(I),MSC(I)
 ENDDO
 OPEN(11,FILE='Line.TXT')
 DO I=1,J
  WRITE(11,*) X(I),Y(I),C(1)+C(2)*X(I)
 ENDDO
 CLOSE(11)

 !WRITE(11,*) C(1),C(2)

 RETURN
END SUBROUTINE


SUBROUTINE TDT(A,NA,B,NB,DT,CC,DDT)
!时间域互相关函数,得到时间差
!A(NA),B(NB):时间序列; DT:采样间隔
!CC:互相关最大值; DDT:时间差(互相关最大值对应的时刻)
 IMPLICIT NONE
 INTEGER*4::I,J,NA,NB
 REAL*4::DT,DDT,A(NA),B(NB),AA,BB,CC
 REAL*4,ALLOCATABLE::C(:)
 ALLOCATE(C(-NA+1:NB-1))
 CALL TCOR(A,NA,B,NB,C,-NA+1,NB-1)
 AA=0.0E0
 BB=0.0E0
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
 !PRINT*, 'J= ',J
 ENDDO
 DDT=J*DT

 OPEN(11,FILE='TCOR.TXT')
 DO I=-NA+1,NB-1
  WRITE(11,*) I*DT,C(I)
 ENDDO
 CLOSE(11)

 RETURN
END SUBROUTINE


SUBROUTINE HPIR1(X,Y,N,A,M,DT1,DT2,DT3)
!最小二乘曲线拟合,来自徐士良《Fortran常用算法程序集-第二版》
!(X(N),Y(N)):数据坐标,A(M):拟合的M-1次多项式的系数
!DT1,DT2,DT3:拟合多项式与数据点偏差的平方和,绝对值之和,绝对值的最大值
 implicit none
 INTEGER*4::N,M,I,J,K
 REAL*4::X(N),Y(N),A(M),S(20),T(20),B(20)
 REAL*4::DT1,DT2,DT3,Z,D1,P,C,D2,G,Q,DT
 A=0.0E0
 IF(M.GT.N) M=N
 IF(M.GT.20) M=20
 Z=0.0E0
 DO I=1,N
  Z=Z+X(I)/N
 ENDDO
 B(1)=1.0E0
 D1=N
 P=0.0E0
 C=0.0E0
 DO I=1,N
  P=P+(X(I)-Z)
	C=C+Y(I)
 ENDDO
 C=C/D1
 P=P/D1
 A(1)=C*B(1)
 IF(M.GT.1) THEN
	T(2)=1.0E0
	T(1)=-P
	D2=0.0E0
	C=0.0E0
	G=0.0E0
	DO I=1,N
	 Q=X(I)-Z-P
	 D2=D2+Q*Q
	 C=Y(I)*Q+C
	 G=(X(I)-Z)*Q*Q+G
  ENDDO
	C=C/D2
	P=G/D2
	Q=D2/D1
	D1=D2
	A(2)=C*T(2)
	A(1)=C*T(1)+A(1)
 ENDIF
 DO J=3,M
	S(J)=T(J-1)
	S(J-1)=-P*T(J-1)+T(J-2)
	IF(J.GE.4) THEN
	 DO K=J-2,2,-1
	  S(K)=-P*T(K)+T(K-1)-Q*B(K)
   ENDDO
	ENDIF
	S(1)=-P*T(1)-Q*B(1)
	D2=0.0E0
	C=0.0E0
	G=0.0E0
	DO I=1,N
	 Q=S(J)
	 DO K=J-1,1,-1
    Q=Q*(X(I)-Z)+S(K)
   ENDDO
	 D2=D2+Q*Q
	 C=Y(I)*Q+C
	 G=(X(I)-Z)*Q*Q+G
  ENDDO
	C=C/D2
	P=G/D2
	Q=D2/D1
	D1=D2
	A(J)=C*S(J)
	T(J)=S(J)
	DO K=J-1,1,-1
	 A(K)=C*S(K)+A(K)
	 B(K)=T(K)
	 T(K)=S(K)
  ENDDO
 ENDDO
 DT1=0.0E0
 DT2=0.0E0
 DT3=0.0E0
 DO I=1,N
	Q=A(M)
	DO K=M-1,1,-1
   Q=Q*(X(I)-Z)+A(K)
  ENDDO
	DT=Q-Y(I)
	IF(ABS(DT).GT.DT3) DT3=ABS(DT)
	DT1=DT1+DT*DT
	DT2=DT2+ABS(DT)
 ENDDO
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


subroutine my_random(x,n,lb,hb)
!生成在区间(lb,hb)均匀分布的随机数组x(n)
 implicit none
 integer::i,n
 real::x(n),t,lb,hb,len
 call random_seed() !系统根据日期和时间随机地提供种子,每次的随机数就都不一样了
 len=hb-lb  !计算范围大小
 do i=1,n
  call random_number(t)  !t是0-1之间的随机数
  x(i)=lb+len*t
 enddo
 return
end subroutine



