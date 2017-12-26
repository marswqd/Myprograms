program mfu
 implicit none
!-----
! 此程序用于多重滤波提取基阶面波群速度频散
! 作者：王清东，武大09研，固体地球物理学
! 时间：2011-09-10 21:33:18
!
! 部分参数说明：
! rhdr    R*4  SAC文件头实型变量
! ihdr    I*4  SAC文件头整型变量
! chdr    C*   SAC文件头字符型变量
! sacname C*   输入的SAC文件名
! tc      R*4  滤波中心周期
! NPTS,DT,B,O,DIST  SAC文件头变量:采样点数,采样间隔,第一个采样点,发震时刻,震中距
! tt      R*4  各采样点走时
! wv      R*4  各采样点对应波速
! TttU    R*4  得到的频散信息
! mT_t,mT_U    R*4  多重滤波波形信息矩阵
!
! 文件说明：
! mfu.in       控制文件:
!              第一行为输入的文件数,多重滤波参数,理论模型的周期与对应相速度 num
!              第二行为滤波起始中心周期,截止周期,周期间隔 btc,etc,dtc
!              第三行为多重滤波参数alpha1 alpha2
!              自第四行一下为输入的文件名 sacname
!
!
! 部分子程序说明：
! brsach,brsac 读取SAC二进制文件信息
! KKFFT        快速傅里叶变换，来源：Fortran常用算法程序集源码-徐士良清华大学
! MFT          多重滤波
!-----
 integer*4::i,j,k,num,NPTS,PNPTS,IRU,nerr
 integer*4::ntc,pl,ph,maxI,nn,flag,nw,pll,phh
 real*4::a,b,alpha,alpha1,alpha2,btc,dtc,etc,dy1,dyn,speak,nrms,snr,tmax,O
 real*4,allocatable::tc(:),tt(:),wv(:),tt1(:),temp1(:),temp2(:)
 real*4,allocatable::mT_t(:,:),mT_U(:,:),TttU(:,:)
 real*4,parameter::plow=2.0e0,phigh=5.0e0      !绘图窗(信号窗)
 real*4,parameter::pai=3.14159265358e0
 character*60::name
 character*60,allocatable::sacname(:)

 real*4,allocatable::data(:),data2(:),dataw(:),dataE(:),IP(:),IT(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)

!读取控制文件信息
 open(11,file='mfu1.in',status='old')         !读取控制文件
 read(11,*) num,flag                          !num:处理的文件数;flag=1 截取信号窗数据进行滤波
 read(11,*) btc,etc,dtc                      !起始周期 终止周期 周期间隔
 read(11,*) alpha1,alpha2                    !起始周期和终止周期对应的alpha值
 allocate(sacname(num))
 do i=1,num
  read(11,*) sacname(i)
  call filesta(sacname(i),nerr)              !检测文件是否存在
  if(nerr.eq.1) then
   print*, 'The file ',trim(sacname(i)),' dosen`t exist'
   goto 99
  endif
  sacname(i)=adjustl(sacname(i))
  print*, trim(sacname(i))
 enddo
 close(11)

 ntc=anint(abs((etc-btc)/dtc))+1             !计算周期数
 allocate(tc(ntc))
 do i=1,ntc
  tc(i)=btc+(i-1)*dtc
 enddo
 !print*, tc(1),tc(ntc),ntc
 a=tc(1)*tc(ntc)/(tc(ntc)-tc(1))*(sqrt(alpha1)-sqrt(alpha2))    !线性时间分辨
 b=(sqrt(alpha2)*tc(ntc)-sqrt(alpha1)*tc(1))/(tc(ntc)-tc(1))

 IRU=100
! DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
! STLA=rhdr(32)  STLO=rhdr(33)  STEL=rhdr(34)  STDP=rhdr(35)
! EVLA=rhdr(36)  EVLO=rhdr(37)  EVEL=rhdr(38)  EVDP=rhdr(39)
! NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
! NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)  KSTNM=chdr(1)
 do i=1,num
  call brsach(IRU,sacname(i),nerr)           !读SAC文件头，提取必要信息
  print*, 'dist= ',rhdr(51)
  NPTS=ihdr(10)
  !对互相关文件,发震时刻O为0
  O=rhdr(8)
  !B=rhdr(6)
  if(O.eq.-12345.0e0) then
   O=0.0e0
   print*, 'the orign time of',sacname(i),'is unknown'
  endif
  tmax=rhdr(51)/10                           !最大可信周期
  ph=anint((rhdr(51)/plow-rhdr(6)+O)/rhdr(1))+1     !确定绘图窗
  pl=anint((rhdr(51)/phigh-rhdr(6)+O)/rhdr(1))+1
  PNPTS=ph-pl+1
  !print*, 'ph=',ph,'pl=',pl
  allocate(tt1(NPTS))
  allocate(mT_t(0:PNPTS,0:ntc),mT_U(0:PNPTS,0:ntc),TttU(ntc,5))
  allocate(tt(PNPTS),wv(PNPTS),temp2(PNPTS))
  mT_t(0,0)=rhdr(51)        !周期-走时数组
  mT_U(0,0)=rhdr(51)        !周期-群速度数组
  do j=1,NPTS
   tt1(j)=rhdr(6)+(pl+j-2)*rhdr(1)
  enddo
  tt(1)=rhdr(6)+(pl-1)*rhdr(1)
  do j=1,PNPTS
   tt(j)=tt(1)+(j-1)*rhdr(1)
   mT_t(j,0)=tt(j)
   wv(j)=phigh+(j-1)*(plow-phigh)/(PNPTS-1)
   mT_U(j,0)=wv(j)
  enddo
  mT_t(0,1:ntc)=tc(1:ntc)
  mT_U(0,1:ntc)=tc(1:ntc)
  !print*,wv1(1),wv(1),wv(2)-wv(1)
!读取波形信息并进行多重滤波
  name=trim(sacname(i))//'.usp'
  !name=trim(chdr(1))//'.usp'
  open(IRU,file=name)
  !open(IRU,file='mfu.out')                  !输出文件
  write(IRU,'(a,a,f8.2)') trim(sacname(i)),'   DIST= ',rhdr(51)
  write(IRU,120)
  120 format('Period'5x'U'5x'STLon'3x'STLat'3x'EVLon'3x'EVLat'5x'SNR'3x'alpha'3x'InstanP')
  allocate(data(NPTS),data2(NPTS))
  call brsac(IRU+1,NPTS,sacname(i),data,nerr)
  do j=1,ntc
   !if(tc(j).gt.tmax) goto 99
   alpha=a/tc(j)+b
   alpha=alpha*alpha
   TttU(j,1)=tc(j)
   data2=data
   allocate(dataE(NPTS),IP(NPTS),IT(NPTS))
   call MFT(data2,NPTS,rhdr(1),tc(j),alpha,dataE,IP,IT) !用于计算谱信噪比
   deallocate(dataE,IP,IT)
   if(flag==1) then
    nw=PNPTS
    pll=1
    phh=PNPTS
    allocate(dataw(nw),dataE(nw),IP(nw),IT(nw),temp1(nw))
    dataw(1:nw)=data(pl:ph)
   else
    nw=NPTS
    pll=pl
    phh=ph
    allocate(dataw(nw),dataE(nw),IP(nw),IT(nw),temp1(nw))
    dataw=data
   endif
   !print*, dataw(1),nw,NPTS,rhdr(1),tc(j),alpha,a,b
   call MFT(dataw,nw,rhdr(1),tc(j),alpha,dataE,IP,IT)      !多重滤波
   !print*, dataE(1)
!计算信噪比
   speak=0.0e0
   do k=pl,ph     !信号窗最大绝对值
    if(speak.lt.abs(data2(k))) speak=abs(data2(k))
   enddo
   nrms=0.0e0
   do k=1,pl      !噪声窗均方根  注:噪声窗定义在信号窗之前
    nrms=nrms+data2(k)*data2(k)
   enddo
   nrms=sqrt(nrms/pl)
   snr=speak/nrms
!寻找群速度对应点
   temp2(1:PNPTS)=dataE(pll:phh)
   !print*, temp2(1),dataE(1)
   call nor(temp2,PNPTS)
   mT_t(1:PNPTS,j)=temp2(1:PNPTS)
   call MAXP(temp2,PNPTS,maxI,nerr)
   TttU(j,2)=tt(maxI)
   TttU(j,3)=rhdr(51)/tt(maxI)
!生成周期-速度矩阵,为绘图准备
   !temp1=dataE
   !call nor(temp1,NPTS)
   !dy1=(temp1(2)-temp1(1))/(tt1(2)-tt1(1))
   !dyn=(temp1(NPTS)-temp1(NPTS-1))/(tt1(NPTS)-tt1(NPTS-1))
   !call espl1(tt1,temp1,NPTS,dy1,dyn,rhdr(51)/wv,PNPTS,temp2)
   !call nor(temp2,PNPTS)
   !mT_U(1:PNPTS,j)=temp2(1:PNPTS)
!寻找瞬时相位
   TttU(j,4)=IP(maxI+pl-1)
!寻找瞬时周期
   TttU(j,5)=IT(maxI+pl-1)
   dataw=data
   write(IRU,'(f6.2,f8.4,2x,4(2xf8.4),3(2xf8.3))') &
      &  TttU(j,1),TttU(j,3),rhdr(33),rhdr(32),rhdr(37),rhdr(36), &
      &  snr,alpha,TttU(j,5)
   deallocate(dataw,dataE,IP,IT,temp1)
  enddo
  deallocate(data,temp2)
  deallocate(mT_t,mT_U,tt,wv,tt1,TttU)
 enddo

 99 continue
 deallocate(sacname,tc)
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


subroutine MFT(data,n,dt,TC,alpha,dataE,IP,IT)
! 多重滤波程序(窄带滤波器为高斯滤波器,截断为pai=3.14159265357e0)
! data:输入输出-波形数据
! dataE:输出-波形振幅(包络)
! IP:输出-瞬时相位(弧度)
! IT:输出-瞬时周期
 implicit none
 integer*4::i,n,logn,nfft
 real*4::data(n),dataE(n),IP(n),IT(n)
 real*4::dt,TC,fc,df,f,alpha,fac,freqlw,frequp,G,omega
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
  omega=2*pai*f
  if(f.ge.freqlw.and.f.le.frequp) then
   G=exp(-alpha*(f/fc-1.0e0)**2)
  else
   G=0.0e0
  endif
  PR(i)=FR(i)*G
  PI(i)=FI(i)*G
  PRD(i)=-omega*PI(i)   !在频率域求导
  PID(i)=omega*PR(i)
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
 call taper(PR,nfft,0.5)
 call taper(PI,nfft,0.5)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,1)
 data(1:n)=2*FR(1:n)
 dataE(1:n)=PR(1:n)
 IP(1:n)=PI(1:n)*2*pai/360.0e0  !弧度
 call taper(PRD,nfft,0.005)
 call taper(PID,nfft,0.005)
 call KKFFT(PRD,PID,nfft,logn,FRD,FID,1,0)
 do i=1,n
  IT(i)=(FR(i)*FID(i)-FRD(i)*FI(i))/(dataE(i)**2)
  IT(i)=2*pai/IT(i)
 enddo
 deallocate(PR,PI,FR,FI,PRD,PID,FRD,FID)
 return
end subroutine


subroutine MAXP(dataE,n,maxI,nerr)
 implicit none
 integer*4::i,n,maxI,nerr
 real*4::dataE(n),maxE
 nerr=0
 maxE=0.0e0
 do i=1,n
  if(maxE.lt.dataE(i)) then
   maxE=dataE(i)
   maxI=i
  endif
 enddo
 !print*, maxI
 return
end subroutine


subroutine espl1(x,y,n,dy1,dyn,xx,m,s)
!subroutine espl1(x,y,n,dy1,dyn,xx,m,dy,ddy,s,ds,dds,t,h)
!给定端点一阶导数的三次样条插值-徐士良
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


subroutine nor(data,n)
 implicit none
 integer*4::n,i
 real*4::data(n),max
 max=-1.0e0
 do i=1,n
  if(max.lt.abs(data(i))) max=abs(data(i))
 enddo
 !print*, max
 if(max.eq.0.0e0) then
  print*, 'This is a zero data, please check'
  return
 endif
 data=data/max
 return
end subroutine

