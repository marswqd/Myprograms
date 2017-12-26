!功能: 带通滤波+时间域归一化+谱白化
!作者: 王清东  时间: 2011-05-14 14:35:37
program btsall
 implicit none
 
 integer*4::i,IRU,nerr,num,NPTS,nfft,logn,wn,wns
 real*4::bt1,bt2,bt3,bt4,t1,t2,t3,t4
 character*2::flag,tn,sw
 character*60::wname
 character*60,allocatable::sacname(:)

 real*4,allocatable::data(:),dfft(:),dfft1(:),dfft2(:),dfft3(:),dfft4(:),dfft5(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 
 open(10,file='info.txt')
 
!读取控制文件信息 
 open(11,file='btsallfile',status='old')
 read(11,*) num                             !num:处理的sac文件数
 do i=1,2
  read(11,*) flag
  if(flag.eq.'bp'.or.flag.eq.'BP') then
   backspace(11)
   read(11,*) flag,bt2,bt3
   bt1=0.9e0*bt2                             !对华北数据:bt2=4.0 bt3=50.0
   bt4=1.1e0*bt3
  else if(flag.eq.'ts'.or.flag.eq.'TS') then
   backspace(11)
   read(11,*) flag,wn,t2,t3,wns              !选择:wn=50 t2=7.0 t3=45.0 wns=50
   t1=0.9e0*t2
   t4=1.1e0*t3
  else
   print*, 'There is something wrong in control info, please check'
   goto 99
  endif
 enddo
 print*, wn,t2,t3,wns
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
  
 t1=0.9e0*t2
 t4=1.1e0*t3
 IRU=100
 do i=1,num
  call brsach(IRU,sacname(i),nerr)        !读SAC文件头，提取必要信息
! DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
! NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
! NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)
  NPTS=ihdr(10)
  allocate(data(NPTS))
  call npow2(NPTS,nfft,logn)
  allocate(dfft(nfft),dfft1(nfft),dfft2(nfft),dfft3(nfft),dfft4(nfft),dfft5(nfft))
  call brsac(IRU,NPTS,sacname(i),data,nerr) !读取波形数据
  dfft=0.0e0
  dfft(1:NPTS)=data(1:NPTS)
  if(rhdr(2).eq.0.0e0.and.rhdr(3).eq.0.0e0) then
   print*, 'This is a ZERO FILE!'
   write(10,*) trim(sacname(i)),' is a ZERO FILE! '
   wname="BBB."//sacname(i)
   wname=adjustl(wname)
   print*, trim(wname)
   write(10,*) trim(wname)
   call wsac(dfft,nfft,wname)
   wname="BOB."//sacname(i)
   wname=adjustl(wname)
   print*, trim(wname)
   write(10,*) trim(wname)
   call wsac(dfft,nfft,wname)
   wname="BOBB."//sacname(i)
   wname=adjustl(wname)
   print*, trim(wname)
   write(10,*) trim(wname)
   call wsac(dfft,nfft,wname)
   wname="BOBS."//sacname(i)
   wname=adjustl(wname)
   print*, trim(wname)
   write(10,*) trim(wname)
   call wsac(dfft,nfft,wname)
   wname="BOBSB."//sacname(i)
   wname=adjustl(wname)
   print*, trim(wname)
   write(10,*) trim(wname)
   call wsac(dfft,nfft,wname)
   wname="BRM."//sacname(i)
   wname=adjustl(wname)
   print*, trim(wname)
   write(10,*) trim(wname)
   call wsac(dfft,nfft,wname)
   wname="BRMB."//sacname(i)
   wname=adjustl(wname)
   print*, trim(wname)
   write(10,*) trim(wname)
   call wsac(dfft,nfft,wname)
   wname="BRMS."//sacname(i)
   wname=adjustl(wname)
   print*, trim(wname)
   write(10,*) trim(wname)
   call wsac(dfft,nfft,wname)
   wname="BRMSB."//sacname(i)
   wname=adjustl(wname)
   print*, trim(wname)
   write(10,*) trim(wname)
   call wsac(dfft,nfft,wname)
   goto 98
  endif
!按地震仪频带宽度进行带通滤波,去处干扰
  call bpf(dfft,nfft,logn,rhdr(1),bt1,bt2,bt3,bt4)
  dfft1=dfft
  dfft2=dfft
  dfft4=dfft
!带通滤波,找寻地震
  call bpf(dfft1,nfft,logn,rhdr(1),t1,t2,t3,t4)
  wname="BBB."//sacname(i)
  wname=adjustl(wname)
  print*, trim(wname)
  write(10,*) trim(wname)
  call wsac(dfft1,nfft,wname)

!onebit,去处地震
  call onebit(dfft2,nfft)                    !时域归一化:onebit
  wname="BOB."//sacname(i)
  wname=adjustl(wname)
  print*, trim(wname)
  write(10,*) trim(wname)
  call wsac(dfft2,nfft,wname)
  dfft3=dfft2
!再次带通滤波,观察地震去处效果
  call bpf(dfft2,nfft,logn,rhdr(1),t1,t2,t3,t4)
  wname="BOBB."//sacname(i)
  wname=adjustl(wname)
  print*, trim(wname)
  write(10,*) trim(wname)
  call wsac(dfft2,nfft,wname)
!谱白化
  !call sonebit(dfft3,nfft,logn,nerr)
  call srunmean(dfft3,nfft,logn,wns,nerr)
  wname="BOBS."//sacname(i)
  wname=adjustl(wname)
  print*, trim(wname)
  write(10,*) trim(wname)
  call wsac(dfft3,nfft,wname)
!再次带通滤波,观察地震去处效果
  call bpf(dfft3,nfft,logn,rhdr(1),t1,t2,t3,t4)
  wname="BOBSB."//sacname(i)
  wname=adjustl(wname)
  print*, trim(wname)
  write(10,*) trim(wname)
  call wsac(dfft3,nfft,wname)

!滑动绝对平均,去处地震
  call runmean(dfft4,nfft,logn,rhdr(1),wn,t1,t2,t3,t4)  !时域归一化:滑动绝对平均法
  wname="BRM."//sacname(i)
  wname=adjustl(wname)
  print*, trim(wname)
  write(10,*) trim(wname)
  call wsac(dfft4,nfft,wname)
  dfft5=dfft4
!再次带通滤波,观察地震去处效果
  call bpf(dfft4,nfft,logn,rhdr(1),t1,t2,t3,t4)
  wname="BRMB."//sacname(i)
  wname=adjustl(wname)
  print*, trim(wname)
  write(10,*) trim(wname)
  call wsac(dfft4,nfft,wname)
!谱白化
  !call sonebit(dfft5,nfft,logn,nerr)
  call srunmean(dfft5,nfft,logn,wns,nerr)
  wname="BRMS."//sacname(i)
  wname=adjustl(wname)
  print*, trim(wname)
  write(10,*) trim(wname)
  call wsac(dfft5,nfft,wname)
!再次带通滤波,观察地震去处效果
  call bpf(dfft5,nfft,logn,rhdr(1),t1,t2,t3,t4)
  wname="BRMSB."//sacname(i)
  wname=adjustl(wname)
  print*, trim(wname)
  write(10,*) trim(wname)
  call wsac(dfft5,nfft,wname)

  98 continue
  deallocate(data,dfft,dfft1,dfft2,dfft3,dfft4,dfft5)
 enddo
  
 99 continue
 deallocate(sacname)
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


subroutine bpf(data,nfft,logn,dt,t1,t2,t3,t4)
 implicit none
 integer::i,logn,nfft
 real*4::dt,df,t1,t2,t3,t4,f1,f2,f3,f4,f,bp
 real*4::data(nfft),PR(nfft),PI(nfft),FR(nfft),FI(nfft)
 real*4,parameter::pai=3.14159265358e0
 call taper(data,nfft,0.005)                !平滑数据的两端,避免吉普斯现象
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
 PR=data
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
 call taper(PR,nfft,0.005)
 call taper(PI,nfft,0.005)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 data=2*FR
 return
end subroutine


subroutine onebit(data,n)
 implicit none
 integer*4::i,n
 real*4::data(n)
 do i=1,n
  if(data(i).gt.0.0e0) then
   data(i)=1.0e0
  else if(data(i).lt.0.0e0) then
   data(i)=-1.0e0
  endif
 enddo
 return
end subroutine


subroutine runmean(data,nfft,logn,dt,wn,t1,t2,t3,t4)
!滑动绝对平均法(running-absolute-mean normalization),权基于带通滤波数据
!来自Processing seismic ambient noise data to obtain reliable
!    broad-band surface wave dispersion measurements    G.D.Bensen(2007)
 implicit none
 integer*4::i,j,nfft,logn,wn,wl,wr
 real*4::data(nfft),dataf(nfft),dt,t1,t2,t3,t4,w
 dataf=data
 call bpf(dataf,nfft,logn,dt,t1,t2,t3,t4)
 do i=1,nfft
  w=0.0e0
  wl=i-wn
  wr=i+wn
  if(wl.lt.1) wl=1
  if(wr.gt.nfft) wr=nfft
  !print*, wl,wr
  do j=wl,wr
   w=w+abs(dataf(j))
  enddo
  w=w/(wr-wl+1)
  data(i)=data(i)/w
 enddo
 return
end subroutine


subroutine sonebit(data,nfft,logn,nerr)
 implicit none
 integer*4::i,logn,nfft,nerr
 real*4::data(nfft),PR(nfft),PI(nfft),FR(nfft),FI(nfft)
 nerr=0
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 call taper(data,nfft,0.005)
 PR=data
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,1)
 !open(14,file='ZZ')
 !do i=1,nn
  !write(14,*) PR(i),PI(i),FR(i),FI(i)
 !enddo
 !close(14)
 do i=1,nfft/2+1
  if(PR(i).eq.0.0e0) then
   nerr=nerr+1
   print*, i,PR(i),PI(i),FR(i),FI(i)
   FR(i)=1.0e0
   FI(i)=0.0e0
  else
   FR(i)=FR(i)/PR(i)
   FI(i)=FI(i)/PR(i)
  endif
 enddo
 PR=FR
 PI=FI
 PR(1)=PR(1)/2
 PI(1)=PI(1)/2
 PR(nfft/2+1)=PR(nfft/2+1)/2
 PI(nfft/2+1)=PI(nfft/2+1)/2
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !令负频率为零
 call taper(PR,nfft,0.005)
 call taper(PI,nfft,0.005)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 !print*, FR(1:2)
 data=2*FR
 return
end subroutine


subroutine srunmean(data,nfft,logn,wn,nerr)
 implicit none
 integer*4::i,j,logn,nfft,wl,wr,wn,nerr
 real*4::data(nfft),PR(nfft),PI(nfft),FR(nfft),FI(nfft),w
 !open(11,file='Spectrum')
 nerr=0
 PR=0.0e0
 PI=0.0e0
 FR=0.0e0
 FI=0.0e0
 call taper(data,nfft,0.005)
 PR=data
 call KKFFT(PR,PI,nfft,logn,FR,FI,0,1)
 call smooth(PR,nfft)
 do i=1,nfft
  w=0.0e0
  wl=i-wn
  wr=i+wn
  if(wl.lt.1) wl=1
  if(wr.gt.nfft) wr=nfft
  !print*, wl,wr
  do j=wl,wr
   w=w+abs(PR(j))
  enddo
  w=w/(wr-wl+1)
  PR(i)=PR(i)/w
 enddo
 do i=1,nfft/2+1
  w=sqrt(FR(i)*FR(i)+FI(i)*FI(i))
  if(w.eq.0.0e0) then
   nerr=nerr+1
   print*, i,FR(i),FI(i)
   FR(i)=1.0e0
   FI(i)=0.0e0
  else
   FR(i)=FR(i)*PR(i)/w
   FI(i)=FI(i)*PR(i)/w
  endif
 enddo
 PR=FR
 PI=FI
 !close(11)
 PR(1)=PR(1)/2
 PI(1)=PI(1)/2
 PR(nfft/2+1)=PR(nfft/2+1)/2
 PI(nfft/2+1)=PI(nfft/2+1)/2
 PR(nfft/2+2:nfft)=0.0e0
 PI(nfft/2+2:nfft)=0.0e0       !令负频率为零
 call taper(PR,nfft,0.005)
 call taper(PI,nfft,0.005)
 call KKFFT(PR,PI,nfft,logn,FR,FI,1,0)
 data=2*FR
 return
end subroutine


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
!生成SAC文件命名(wname):台站.年日.SAC
 implicit none
 integer*4::i,n,len
 real*4::min,max,sum
 real*4::data(n)
 character*(*)::wname                 !由于字符长度固定,实用性不强,可能出错
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


