program test
 implicit none
!-----
! 此程序用于提取基阶面波相速度频散信息      
! 作者：王清东，武大09研，固体地球物理学
!
! 参数说明：
! ntp     I*4  经窄带滤波得到的SAC文件个数
! NPTS    I*4  SAC时间序列文件的采样点数
! LN      I*4  读取的SAC时间序列文件的采样点数
! rhdr    R*4  SAC文件头实型变量
! ihdr    I*4  SAC文件头整型变量
! chdr    C*   SAC文件头字符型变量
! sacname C*   经窄带滤波得到的SAC文件名 
! tp      R*4  窄带滤波中心周期
! t0,v0   R*4  输入的理论模型的周期与对应波速
! NPTS,DT,B,O,DIST  SAC文件头变量:采样点数,采样间隔,第一个采样点,发震时刻,震中距
! sacdata R*4  SAC文件振幅信息
! tt      R*4  各采样点走时
! wv      R*4  各采样点对应波速
! pv      R*4  得到的频散信息
! sacpv   R*4  频散信息矩阵
! tb,vb   I*4  搜索到的频散点
!
! 文件说明：
! controlfile  控制文件:
!              第一行为输入的理论模型的周期与对应波速 t0,v0
!              第二行为文件名 sacname
!              第三行为窄带滤波起始中心周期,截止周期,周期间隔 btp,etp,dtp
!
! 子程序说明：
! brsac        读取SAC二进制文件信息
! KKFFT        快速傅里叶变换，来源：Fortran常用算法程序集源码-徐士良清华大学
! nbf          窄带滤波
! pointing     将波峰标记为1.0
! band         搜索与输入的理论点最接近的频散点
! search       搜索全部的频散点
! putshd       波形灰度图
! putsac       波形图+得到的频散信息图
!-----
 integer*4::i,j,k,tb,vb,bb,nerr
 integer*4::ntp,NPTS,N,NN
 character*20::sacname
 real*4::t0,v0,btp,dtp,etp,TC,dtpl,nbm,nbn
 real*4::B,O,DT,DIST
 real*4,allocatable::tp(:),tpl(:),saccut(:),tt(:),wv(:)
 real*4,allocatable::PR(:),PI(:),FR(:),FI(:),RR(:),II(:),nb(:)
 real*4,allocatable::sacdata(:,:),sacpv(:,:),pv(:,:) 
 real*4,parameter::vs=3.0,ve=5.0
 real*4,parameter::author=87.8168
 
 integer*4,parameter::LN=1000000
 real*4::data(LN)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 
 real*4::xaxlen,yaxlen,x0,y0,hlen
 real*4::firstx,deltax,firsty,deltay
 real*4,allocatable::sac(:)
 integer*4::pnum

 
!读取控制文件信息 
 open(10,file='controlfile',status='old')
 read(10,*) t0,v0
 read(10,*) sacname
 sacname=trim(adjustl(sacname))
 !print*, sacname
 read(10,*) btp,etp,dtp
 !print*, btp,etp,dtp
 close(10)

  
!读取SAC文件并剪取信息到saccut
 call brsac(11,LN,sacname,data,nerr)
 NPTS=ihdr(10)
 DT=rhdr(1)
 B=rhdr(6)
 O=rhdr(8)
 DIST=rhdr(51)
 print*, NPTS,DT,B,O,DIST
 i=floor(abs((DIST/vs-(B-O))/DT))+1
 j=floor(abs((DIST/ve-(B-O))/DT))
!vs<ve:i>j
 NPTS=abs(i-j+1)
 B=(B-O)+(j-1)*DT
 ntp=floor(abs((etp-btp)/dtp))+1
 print*, NPTS,DT,B,O,DIST
 allocate(saccut(NPTS))
 allocate(tt(NPTS))
 allocate(wv(NPTS))
 allocate(tp(ntp))
 allocate(tpl(ntp))
 dtpl=abs(log10(etp)-log10(btp))/ntp 
 allocate(sacdata(0:NPTS,-1:ntp))
 sacdata(0,-1)=DIST
 sacdata(0,0)=author
 do k=1,NPTS
  saccut(k)=data(k+j-1)
  tt(k)=B+(k-1)*DT
  wv(k)=DIST/tt(k)
  sacdata(k,-1)=tt(k)
  sacdata(k,0)=wv(k)
 enddo  
 !print*, (tt(k),k=1,5)
 do k=1,ntp
  tp(k)=btp+(k-1)*dtp
  tpl(k)=log10(btp)+(k-1)*dtpl
  tpl(k)=10**tpl(k)
  !print*, tp(k)
 enddo
 !sacdata(0,1:ntp)=tp(1:ntp)
 sacdata(0,1:ntp)=tpl(1:ntp)  


!序列补零+FFT+nbf+IFFT,保存滤波信息到sacdata 
 NN=floor(log10(NPTS+0.0)/log10(2.0))+1
 N=2**NN
 print*, NN,N
 allocate(PR(N)) 
 allocate(PI(N))
 allocate(FR(N)) 
 allocate(FI(N))
 allocate(RR(N)) 
 allocate(II(N))
 PR=0.0
 PI=0.0
 FR=0.0
 FI=0.0
 RR=0.0
 II=0.0
 PR(1:NPTS)=saccut(1:NPTS)
 !print*, 'saccut=',(saccut(k),k=1,5)
 !print*, 'PR=',(PR(k),k=NPTS,NPTS+5)
 call KKFFT(PR,PI,N,NN,FR,FI,0,0)
 !print*, 'PR=',(PR(k),k=1,5)
 RR=FR
 II=FI
 open(20,file='nbf')
 allocate(nb(N)) 
 do i=1,ntp
  !TC=tp(i)
  TC=tpl(i)
  if(TC.le.40.0) then
   nbm=1.0
   nbn=0.2
  elseif(TC.gt.40.0.and.TC.lt.80.0) then 
   nbm=1.8
   nbn=0.13
  else
   nbm=2.6
   nbn=0.35
  endif
  call nbf(nb,N,0.0,1.0/N,1.0/TC,nbm,nbn)
  write(20,*) TC,nb
  do j=1,N
   PR(j)=RR(j)*nb(j)
   PI(j)=II(j)*nb(j)
  enddo
  call KKFFT(PR,PI,N,NN,FR,FI,1,1)
  !print*, 'FI=',(FI(k),k=1,5)
  !print*, 'FR=',(FR(k),k=NPTS,NPTS+5)
  sacdata(1:NPTS,i)=FR(1:NPTS)
 enddo
 close(20)
 
 open(600,file='sacdata')
 do i=0,NPTS
  write(600,*) sacdata(i,:)
 enddo
 close(600)
  
  
!搜索相速度频散信息并保存到savpv和pv中 
 allocate(pv(ntp+2,5))
 allocate(sacpv(0:NPTS,-1:ntp))
 sacpv=sacdata
 sacpv(1:NPTS,1:ntp)=0.0
 call pointing(sacdata,sacpv,NPTS,ntp)
 !call band(tp,ntp,t0,tb)
 call band(tpl,ntp,t0,tb)
 call band(tt,NPTS,DIST/v0,vb)
 print*, tb,vb
 pv(ntp+1,:)=12345.0
 pv(ntp+2,1)=t0
 pv(ntp+2,2)=v0
 pv(ntp+2,3)=DIST/v0
 pv(ntp+2,4)=vb
 pv(ntp+2,5)=tb
 bb=vb
 do k=tb,1,-1
  call search(sacpv,NPTS,ntp,k,vb)
  !print*, k,vb
  !print*, sacpv(vb,k)
  !pv(k,1)=tp(k)
  pv(k,1)=tpl(k)
  pv(k,2)=wv(vb)
  pv(k,3)=tt(vb) 
  pv(k,4)=vb
  pv(k,5)=k
  sacpv(vb,k)=author
 enddo
 do k=tb+1,ntp
  call search(sacpv,NPTS,ntp,k,bb)
  !print*, k,bb
  !print*, sacpv(bb,k)
  !pv(k,1)=tp(k)
  pv(k,1)=tpl(k)
  pv(k,2)=wv(bb)
  pv(k,3)=tt(bb)
  pv(k,4)=bb
  pv(k,5)=k
  sacpv(bb,k)=author
 enddo 

 open(700,file='sacpv')
 open(800,file='pv')
 do i=0,NPTS
  write(700,*) sacpv(i,:)
 enddo
 do i=1,ntp+2
  write(800,*) pv(i,:)
 enddo
 close(700)
 close(800)

 
!图示化功能实现
 call pinitf('test.plt')
 call gunit('in')
 call factor(0.9)
 
 xaxlen=9.0
 yaxlen=6.0
 !firstx=tp(1)
 !deltax=(tp(ntp)-tp(1))/xaxlen
 k=2
 call pltscl(tp,xaxlen,ntp,firstx,deltax,k)
 firsty=wv(NPTS)
 deltay=(wv(1)-wv(NPTS))/yaxlen
 hlen=dtpl/(2.0*deltax)
 
 call plot(0.5,1.0,-3)
 !allocate(sac(NPTS))
 !do j=1,ntp
  !do i=1,NPTS
   !sac(i)=sacdata(i,j)
  !enddo
  !x0=(tp(j)-tp(1))/deltax
  !y0=0.0
  !pnum=pv(j,4)
  !call putshd(x0,y0,sac,wv,NPTS,hlen,firsty,deltay,j)
  !call putsac(x0,y0,sac,wv,NPTS,0.8*hlen,firsty,deltay,pnum,j)   
 !enddo
 !call putpv(pv,ntp+2,5,firstx,deltax,firsty,deltay)
 
 
 
 call algaxe(xaxlen,yaxlen,k,0,'Period/s',       &
             'Phase Velocity/km*s-1',8,21,firstx,firsty,deltax,deltay)
 !call axis(0.0,0.0,'Period/s',-8,xaxlen,0.0,firstx,deltax)
 !call axis(0.0,0.0,'Phase Velocity/km*s-1',21,yaxlen,90.0,firsty,deltay)
 !call graysc(9.5,0.0,0.5,yaxlen)
 print*, firstx,deltax
 
 call pend()

 
end program


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
end


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
 PR(1)=1.0
 PI(1)=0.0
 PR(2)=COS(6.283185306/N)
 PI(2)=-SIN(6.283185306/N)
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
	 PI(I)=ATAN(FI(I)/FR(I))*360.0/6.283185306
  ENDDO
 ENDIF
 RETURN
END


subroutine nbf(nb,NPTS,B,DT,TC,m,n)
 implicit none
 integer::i,NPTS
 real*4::m,n,B,DT,TC,T1,T2,T3,T4,t
 real*4::nb(NPTS)
 real*4,parameter::pi=3.141592653574
 !print*, B,NPTS,DT,T0,n,m
 T1=TC-(m+n)*DT
 T2=TC-n*DT
 T3=TC+n*DT
 T4=TC+(m+n)*DT
 do i=1,NPTS
  t=B+(i-1)*DT
  if(t.GE.T1.AND.t.LE.T2) then
   nb(i)=0.5*(cos(pi*(t-T2)/(T2-T1))+1.0)
  elseif(t.GT.T2.AND.t.LT.T3) then
   nb(i)=1.0
  elseif(t.GE.T3.AND.t.LE.T4) then
   nb(i)=0.5*(cos(pi*(t-T3)/(T4-T3))+1.0)
  else
   nb(i)=0.0
  endif
 enddo
 return
end


subroutine pointing(x,y,m,n)
 implicit none
 integer*4::i,j,m,n
 real*4::x(0:m,-1:n),y(0:m,-1:n),a,b,c
 c=0.0 
 do j=1,n
  a=x(2,j)-x(1,j)
  y(1,j)=0.0
  y(m,j)=0.0
  do i=2,m-1
   b=x(i+1,j)-x(i,j)
   if(a.ge.c.and.b.le.c) then
    y(i,j)=1.0
   else
    y(i,j)=0.0
   endif
   a=b
  enddo
 enddo
 return 
end


subroutine band(x,m,x0,lb)
 implicit none
 integer*4::lb,i,m
 real*4::x(m),a,b,c,x0
 a=x(1)-x0
 c=0.0
 do i=1,m-1
  b=x(i+1)-x0
  if(a*b.le.c) then
   if(abs(a).gt.abs(b)) then
    lb=i+1
   else
    lb=i
   return
   endif
  endif
  a=b
 enddo
 return
end
  

subroutine search(x,m,n,xb,yb)
 implicit none
 integer*4::i,j,xb,yb,m,n
 real*4::x(0:m,-1:n),a,b 
 do i=yb,1,-1
  a=x(i,xb)
  if(a==1.0) exit
 enddo
 do j=yb,m
  b=x(j,xb)
  if(b==1.0) exit
 enddo
 if(i==1) then
  yb=j 
  return
 endif
 if(j==m) then
  yb=i
  return
 endif
 if(abs(yb-i).le.abs(j-yb)) then
  yb=i
 else
  yb=j
 endif
 return
end


subroutine putsac(x0,y0,x,y,n,hlen,firsty,deltay,pnum,j)
 implicit none
 integer*4::i,j,pnum,n
 real*4::x(n),y(n),x0,y0
 real*4::amp,xx,yy,hlen,firsty,deltay 
 amp=-1.0
 do i=1,n
	if(amp.lt.abs(x(i))) amp=abs(x(i))
 enddo
 call plot(x0,y0,-3)
 do i=1,n
  xx=x(i)/amp*hlen
  if(j.eq.1.and.xx.lt.0.0) xx=0.0
  yy=(y(i)-firsty)/deltay
  if(i.eq.1)then
   call plot(xx,yy,3)
  else
   call plot(xx,yy,2)
  endif
  if(i.eq.pnum) then
   call newpen(1100)
   call symbol(xx-0.04,yy-0.08,0.1,'*',0.0,1)
   call newpen(1)
  endif
 enddo
 call plot(-x0,-y0,-3)
 return
end


subroutine putshd(x0,y0,x,y,n,hlen,firsty,deltay,j)
 implicit none
 integer*4::n,i,j,ipen
 real*4::x(n),y(n),x0,y0,pen,ampmin,ampmax,amp
 real*4::xl,xh,yl,yh,hlen,firsty,deltay
 ampmin=1.0e+38
 ampmax=-1.0e+38
 do i=1,n
	if(ampmin.gt.x(i)) ampmin=x(i)
	if(ampmax.lt.x(i)) ampmax=x(i)
 enddo
 amp=ampmax-ampmin
 call plot(x0,y0,-3)
 do i=2,n
  if(j.eq.1) then
   xl=0.0
  else
   xl=-hlen
  endif
  xh=hlen
  yl=(y(i-1)-firsty)/deltay
  yh=(y(i)-firsty)/deltay
  pen=1100.0-(((x(i-1)+x(i))/2.0-ampmin)/amp)*100.0
  if(pen.lt.1000.0) pen=1000.0
  if(pen.gt.1100.0) pen=1100.0
  ipen=floor(pen)
  !print*, ipen
!	ipen = 1000 is red, 1100 = blue or 1000 = dark, 1100 = light halftone
  call newpen(ipen)
  call shader(xl,yl,xh,yh,0,0,0.02,0.02)
 enddo
 call newpen(1)
 call plot(-x0,-y0,-3)
 return
end


subroutine putpv(x,m,n,firstx,deltax,firsty,deltay)
 implicit none
 integer*4::i,m,n
 real*4::x(m,n),xx,yy,firstx,deltax,firsty,deltay
 do i=1,m-2
  xx=(x(i,1)-firstx)/deltax
  yy=(x(i,2)-firsty)/deltay
  if(i.eq.1)then
   call plot(xx,yy,3)
  else
   call plot(xx,yy,2)
  endif
 enddo
 xx=(x(m,1)-firstx)/deltax
 yy=(x(m,2)-firsty)/deltay
 call symbol(xx,yy,0.1,'.',0.0,1)
 return
end


subroutine graysc(x0,y0,xlen,ylen)
 implicit none
 integer*4::i,ipinc,ipen
 real*4::x0,y0,xlen,ylen,dy,xl,xh,yl,yh,pen
 call plot(x0,y0,-3)
 call plot(0.0,ylen,2)
 call plot(xlen,ylen,2)
 call plot(xlen,0.0,2)
 call plot(0.0,0.0,2)
 ipinc=11
 dy=ylen/ipinc
 do i=1,ipinc
  xl=0.0
  xh=xlen
  yl=(i-1)*dy
  yh=i*dy
  pen=1100.0-(i-1)*100.0/(ipinc-1)
  ipen=floor(pen)
  call newpen(ipen)
  call shader(xl,yl,xh,yh,0,0,0.01,0.01)
  call newpen(1)
  call number(xh+0.1,yh-0.6*dy,0.1,0.1*(i-1),0.0,1)
 enddo
 call plot(-x0,-y0,-3)
 return
end
