program test
 implicit none
!-----
! 此程序用于提取基阶面波相速度频散信息      
! 作者：王清东，武大09研，固体地球物理学
!
! 参数说明：
! sacnum  I*4  经窄带滤波得到的SAC文件个数
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
! controlfile  控制文件，第一行为输入的理论模型的周期与对应波速 t0,v0
!                        第二行为文件个数
!                        自第三行为以下为文件名和对应窄带滤波中心周期 sacname/tp
!
! 子程序说明：
! brsac        读取SAC二进制文件信息
! band         搜索与输入的理论点最接近的频散点
! search       搜索全部的频散点
! pointing     将波峰标记为1.0
! putshd       波形灰度图
! putsac       波形图+得到的频散信息图
!-----
 integer*4::i,j,k,tb,vb,bb,nerr
 integer*4::sacnum,NPTS
 real*4::t0,v0
 real*4::B,O,DT,DIST
 real*4,allocatable::tp(:),tt(:),wv(:),pv(:,:)
 real*4,allocatable::sacdata(:,:),sacpv(:,:)
 character*20,allocatable::sacname(:)
 real*4,parameter::author=87.8168
 
 integer*4,parameter::LN=1000000
 real*4::data(LN)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 
 real*4::xaxlen,yaxlen,x0,y0
 real*4::firstx,deltax,firsty,deltay
 real*4,allocatable::sac(:)
 integer*4::pnum

 
!读取控制文件信息 
 open(10,file='controlfile',status='old')
 read(10,*) t0,v0
 read(10,*) sacnum
 print*, sacnum
 allocate(sacname(sacnum))
 allocate(tp(sacnum))
 do i=1,sacnum
  read(10,*) sacname(i)
  !print*, sacname(i)
  sacname(i)=trim(adjustl(sacname(i)))
  !print*, sacname(i)
  read(10,*) tp(i)
 enddo 
 close(10)

  
!读取SAC文件信息到sacdata
 i=1
 call brsac(11,LN,sacname(i),data,nerr)
 NPTS=ihdr(10)
 DT=rhdr(1)
 B=rhdr(6)
 O=rhdr(8)
 DIST=rhdr(51)
 print*, NPTS,DT,B,O,DIST
 allocate(sacdata(0:NPTS,-1:sacnum))
 allocate(tt(NPTS))
 allocate(wv(NPTS))
 sacdata(0,-1)=DIST
 sacdata(0,0)=author
 do i=1,sacnum
  sacdata(0,i)=tp(i)
 enddo
 do i=1,NPTS
  tt(i)=(B-O)+(i-1)*DT
  wv(i)=DIST/tt(i)
  sacdata(i,-1)=tt(i)
  sacdata(i,0)=wv(i)
 enddo
 
 do i=1,sacnum
  j=i+10
  call brsac(j,LN,sacname(i),data,nerr)
  do k=1,NPTS
   sacdata(k,i)=data(k)
  enddo
  !print*, rhdr(3)
 enddo
 
 open(600,file='sacdata')
 do i=0,NPTS
  write(600,*) sacdata(i,:)
 enddo
 close(600)


!搜索相速度频散信息并保存到savpv和pv中 
 allocate(pv(sacnum+2,4))
 allocate(sacpv(0:NPTS,-1:sacnum))
 sacpv=sacdata
 sacpv(1:NPTS,1:sacnum)=0.0
 call pointing(sacdata,sacpv,NPTS,sacnum)
 call band(tp,sacnum,t0,tb)
 call band(tt,NPTS,DIST/v0,vb)
 print*, tb,vb
 pv(sacnum+1,:)=0.0
 pv(sacnum+2,1)=t0
 pv(sacnum+2,2)=v0
 pv(sacnum+2,3)=vb
 pv(sacnum+2,4)=tb
 bb=vb
 do k=tb,1,-1
  call search(sacpv,NPTS,sacnum,k,vb)
  !print*, k,vb
  !print*, sacpv(vb,k)
  pv(k,1)=tp(k)
  pv(k,2)=wv(vb) 
  pv(k,3)=vb
  pv(k,4)=k
  sacpv(vb,k)=author
 enddo
 do k=tb+1,sacnum
  call search(sacpv,NPTS,sacnum,k,bb)
  !print*, k,bb
  !print*, sacpv(bb,k)
  pv(k,1)=tp(k)
  pv(k,2)=wv(bb)
  pv(k,3)=vb
  pv(k,4)=k
  sacpv(bb,k)=author
 enddo 

 open(700,file='sacpv')
 open(800,file='pv')
 do i=0,NPTS
  write(700,*) sacpv(i,:)
 enddo
 do i=1,sacnum
  write(800,*) pv(i,:)
 enddo
 close(700)
 close(800)

 
!图示化功能实现
 call pinitf('test.plt')
 call gunit('in')
 call factor(0.9)
 
 xaxlen=10.0
 yaxlen=5.0
 firstx=0.0
 deltax=20.0
 firsty=tt(NPTS)
 deltay=(tt(1)-tt(NPTS))/yaxlen
 
 call plot(0.5,0.5,-3)
 call axis(0.0,0.0,'Period/s',-8,xaxlen,0.0,firstx,deltax)
 call axis(0.0,0.0,'Travel Time/s',13,yaxlen,90.0,firsty,deltay)
 
 allocate(sac(NPTS))
 do j=1,sacnum
  do i=1,NPTS
   sac(i)=sacdata(i,j)
  enddo
  x0=0.75+j*0.25
  y0=0.0
  pnum=pv(j,3)
  call putshd(x0,y0,sac,tt,NPTS,0.125,firsty,deltay,pnum)
  call putsac(x0,y0,sac,tt,NPTS,0.1,firsty,deltay,pnum)
   
 enddo
 








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


subroutine putsac(x0,y0,x,y,n,hlen,firsty,deltay,pnum)
 implicit none
 integer*4::i,pnum,n
 real*4::x(n),y(n),x0,y0
 real*4::amp,xx,yy,hlen,firsty,deltay 
 amp=-1.0
 do i=1,n
	if(amp.lt.abs(x(i))) amp=abs(x(i))
 enddo
 call plot(x0,y0,-3)
 do i=1,n
  xx=x(i)/amp*hlen
  yy=(y(i)-firsty)/deltay
  if(i.eq.1)then
   call plot(xx,yy,3)
  else
   call plot(xx,yy,2)
  endif
  if(i.eq.pnum) then
   call newpen(1100)
   call symbol(xx-0.04,yy-0.07,0.1,'*',0.0,1)
   call newpen(1)
  endif
 enddo
 call plot(-x0,-y0,-3)
 return
end


subroutine putshd(x0,y0,x,y,n,hlen,firsty,deltay,pnum)
 implicit none
 integer*4::n,i,pnum,ipen
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
  xl=-hlen
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