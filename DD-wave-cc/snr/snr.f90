program snr
!-----------------------------------------------------------------------------!
!--
!--The function of this program is select the event records with high SNR
!--
!--Author:Wang Qingdong, School of Geodesy and Geomatics, Wuhan University,
!--Wuhan City, Hubei Province, China. 430079.
!--E-mail: wqd1986@whu.edu.cn
!--Creatied time: 2014/4/14 20:15:44
!--Modified time: 2014/4/14 20:15:47
!--
!-----------------------------------------------------------------------------!
 implicit none
 integer*4::i,ii,j,k,n,ns,num,nerr,NPTS,nratio
 integer*4::nn1,nn2,np1,np2,ns1,ns2
 character::nmz*100,nmr*100,nmt*100,cmd*200
 character,allocatable::sacname(:)*100
 real*4::B,O,P,S,DTT,dist,mag
 real*4::zn,rn,tn,zp,rp,tp,zs,rs,ts,zsp,rsp,tsp,zss,rss,tss
 real*4::sld,snrp,snrs,zsp0,rsp0,tsp0,zss0,rss0,tss0

 real*4,allocatable::dz(:),dr(:),dt(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)


 open(10,file='snr.log')                 ! information file of the program
 open(11,file='snr.in',status='old',action='read')
 read(11,*) num
 allocate(sacname(num))
 do i=1,num
   read(11,*) sacname(i)
   j=index(sacname(i),'.Z.')
   if(j==0) then
    print*, 'The input is not Z component, please check!'
    stop
   endif
   nmz=sacname(i)(1:j)//'Z.SAC'
   nmr=sacname(i)(1:j)//'R.SAC'
   nmt=sacname(i)(1:j)//'T.SAC'
   call exist(nmz)
   call exist(nmr)
   call exist(nmt)
   call exist(sacname(i))
   sacname(i)=adjustl(sacname(i))
   print*, trim(sacname(i))
 enddo
 close(11)


 nratio=0
 do i=1,num
   !--read the head of the sac file
   call brsach(100,sacname(i),nerr)
   !-------------------------------------------------------------
   !  DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
   !  NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
   !  NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)
   !  O=rhdr(8) A=rhdr(9)
   !  eqla=rhdr(36)  eqlon=rhdr(37)          eqdepth=rhdr(39)
   !  mag=rhdr(40)
   !  min=rhdr(2)    max=rhdr(3)
   !  KA=chdr(6),the type of A.
   !  Here A=rhdr(9) is the travel time of P wave
   !-------------------------------------------------------------
   NPTS=ihdr(10)
   DTT=rhdr(1)     !采样间隔
   B=rhdr(6)     !第一个采样点的时刻
   O=rhdr(8)     !发震时刻
   P=rhdr(9)     !A震相到时,一般为P波到时
   S=rhdr(11)    !T0震相到时,一般为S波到时
   dist=rhdr(51) !震中距
	 mag=rhdr(40)
	 k=index(sacname(i),'.Z.')
   nmz=sacname(i)(1:k)//'Z.SAC'
   nmr=sacname(i)(1:k)//'R.SAC'
   nmt=sacname(i)(1:k)//'T.SAC'
   !--read the data of the sac file
   !print*, nmz,P,S
   allocate(dr(NPTS),dt(NPTS),dz(NPTS))
   call brsac(100,NPTS,nmz,dz,nerr)
   call brsac(100,NPTS,nmr,dr,nerr)
   call brsac(100,NPTS,nmt,dt,nerr)
   !--calculate the SNR of the data,and choose the data that have
   !--enough SNR. The SNR is the ratio of P and S wave window and
   !--the noise (O to P-1). The snr is root mean squre amplitude ratio
   nn1=nint((O-B)/DTT)+1
   nn2=nint((P-1-B)/DTT)+1   !noise window
   if(nn2<nn1) nn2=nn1+nint(0.5e0/DTT)+1  !at lest 0.5s
   !noise
   zn=0.0e0
   rn=0.0e0
   tn=0.0e0
   do j=nn1,nn2
     zn=zn+dz(j)**2
     rn=rn+dr(j)**2
     tn=tn+dt(j)**2
   enddo
   zn=sqrt(zn/(nn2-nn1+1))
   rn=sqrt(rn/(nn2-nn1+1))
   tn=sqrt(tn/(nn2-nn1+1))
	 sld=1.0e0
   ns=nint(sld*2/DTT)
	 !P
   if(P/=-12345.0e0) then
		 snrp=0.0e0
		 n=nint(1.5e0/DTT)
		 np1=nint((P-0.5e0-B-sld)/DTT)+1
     np2=nint((P+1.0e0-B+sld)/DTT)+1 !P window
     zp=0.0e0
     rp=0.0e0
     tp=0.0e0
		 do ii=0,ns
		   do j=np1+ii,np1+ii+n
         zp=zp+dz(j)**2
         rp=rp+dr(j)**2
         tp=tp+dt(j)**2
       enddo
       zp=sqrt(zp/(n+1))
       rp=sqrt(rp/(n+1))
       tp=sqrt(tp/(n+1))
       !snr
       zsp0=zp/zn
       rsp0=rp/rn
       tsp0=tp/tn
			 if(snrp<zsp0+rsp0) then
				 snrp=zsp0+rsp0
         zsp=zsp0
         rsp=rsp0
         tsp=tsp0
			 endif
		 enddo
   else
     zsp=-12345.0e0
     rsp=-12345.0e0
     tsp=-12345.0e0
     snrp=zsp+rsp
   endif
   if(S/=-12345.0e0) then
		 snrs=0.0e0
		 n=nint(2.5e0/DTT)
		 ns1=nint((S-0.5e0-B-sld)/DTT)+1
     ns2=nint((S+1.0e0-B+sld)/DTT)+1 !S window
     zs=0.0e0
     rs=0.0e0
     ts=0.0e0
     do ii=0,ns
		   do j=ns1+ii,ns1+ii+n
         zs=zs+dz(j)**2
         rs=rs+dr(j)**2
         ts=ts+dt(j)**2
       enddo
       zs=sqrt(zs/(n+1))
       rs=sqrt(rs/(n+1))
       ts=sqrt(ts/(n+1))
       !snr
       zss0=zs/zn
       rss0=rs/rn
       tss0=ts/tn
			 if(snrs<tss0) then
         snrs=tss0
         zss=zss0
         rss=rss0
         tss=tss0
			 endif
		 enddo
   else
     zss=-12345.0e0
     rss=-12345.0e0
     tss=-12345.0e0
     snrs=tss
	 endif
   if(snrp>=10.0e0.or.snrs>=30.0e0) then
     nratio=nratio+1
     write(10,*) sacname(i)(1:k),'  ',dist,mag,'select'
     write(10,*) zsp,rsp,tsp
     write(10,*) zss,rss,tss
   else
     write(10,*) sacname(i)(1:k),'  ',dist,mag
     write(10,*) zsp,rsp,tsp
     write(10,*) zss,rss,tss
   endif
   !if(snrp>=10.0e0.or.snrs>=30.0e0) then
		 !cmd='mv '//sacname(i)(1:k)//'*.SAC  CCSACall/'
		 !print*, cmd
		 !call system(cmd)
	 !endif
   deallocate(dz,dr,dt)
 enddo
 print*, nratio*1.0e0/num
 write(10,*) 'select ratio: ',nratio*1.0e0/num


 close(10)



 99 continue
end program


subroutine exist(fn)
 implicit none
 character*(*)::fn
 logical::ex
 inquire(file=fn,exist=ex)
 if(.not.ex) then
  write(*,'("file does not exist ",a)') trim(fn)
  stop
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
 open(IRU,file=name,form='unformatted',access='direct',recl=440,status='old')
 read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40)
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
