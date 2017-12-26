!功能: 选取(selection)双边互相关信号(signal)的某一边生成新的SAC文件
!作者: 王清东 时间：2011-05-25 10:15:15
program ss
 implicit none

 integer*4::i,j,IRU,nerr,num,NPTS,NPTSS,zero
 real*4::EVLA,EVLO,beignA,T1,T2             !保存头文件变量
 character*1,allocatable::dir(:),inver(:)
 character*60::wname
 character*60,allocatable::sacname(:)

 real*4,allocatable::data(:),dataS(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)

 call CPU_TIME(T1)
 open(10,file='info.txt')                   !程序运行信息文件

!读取控制文件信息
 open(11,file='ssfile',status='old')        !ssfile中存放需要进行操作的文件信息
 !dir=-:只提取非因果信号
 !dir=a:将双端(因果+非因果)信号变为平均信号
 !dir=+:只提取因果信号
 !inver=i:将得到的信号反向(DT=-DT)
 !inver=n:不反向
 read(11,*) num
 allocate(sacname(num),dir(num),inver(num))
 do i=1,num
  read(11,*) dir(i),inver(i),sacname(i)
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
 do i=1,num
!  DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
!  STLA=rhdr(32)  STLO=rhdr(33)  STEL=rhdr(34)  STDP=rhdr(35)
!  EVLA=rhdr(36)  EVLO=rhdr(37)  EVEL=rhdr(38)  EVDP=rhdr(39)
!  NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
!  NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)  KSTNM=chdr(1)
  call brsach(IRU,sacname(i),nerr)
  NPTS=ihdr(10)
  allocate(data(NPTS))
  zero=anint(-rhdr(6)/rhdr(1))+1
  print*, 'zero=',zero
  call brsac(IRU,NPTS,sacname(i),data,nerr)
  if(dir(i).eq.'-') then
   NPTSS=zero
   allocate(dataS(NPTSS))
   do j=1,NPTSS
    dataS(j)=data(NPTSS-j+1)
   enddo
   rhdr(6)=0.0e0
   if(inver(i).eq.'i'.or.inver(i).eq.'I') rhdr(1)=-rhdr(1)
   rhdr(7)=rhdr(6)+(NPTSS-1)*rhdr(1)
   wname=trim(sacname(i))//'.-'
   wname=trim(adjustl(wname))               !wname:生成的SAC文件名
   print*, wname
   !call nor(dataS,NPTSS)
   call wsac(dataS,NPTSS,wname)              !生成新的SAC文件
   deallocate(dataS)
  else if(dir(i).eq.'+') then
   NPTSS=NPTS-zero+1
   allocate(dataS(NPTSS))
   do j=1,NPTSS
    dataS(j)=data(NPTS-NPTSS+j)
   enddo
   rhdr(6)=0.0e0
   if(inver(i).eq.'i'.or.inver(i).eq.'I') rhdr(1)=-rhdr(1)
   rhdr(7)=rhdr(6)+(NPTSS-1)*rhdr(1)
   wname=trim(sacname(i))//'.+'
   wname=trim(adjustl(wname))               !wname:生成的SAC文件名
   print*, wname
   !call nor(dataS,NPTSS)
   call wsac(dataS,NPTSS,wname)              !生成新的SAC文件
   deallocate(dataS)
  else if(dir(i).eq.'a'.or.dir(i).eq.'A') then
   NPTSS=min(zero,NPTS-zero+1)
   allocate(dataS(NPTSS))
   dataS(1)=data(zero)
   do j=2,NPTSS
    dataS(j)=(data(zero-j+1)+data(zero+j-1))/2
   enddo
   rhdr(6)=0.0e0
   if(inver(i).eq.'i'.or.inver(i).eq.'I') rhdr(1)=-rhdr(1)
   rhdr(7)=rhdr(6)+(NPTSS-1)*rhdr(1)
   wname=trim(sacname(i))//'.A'
   wname=trim(adjustl(wname))               !wname:生成的SAC文件名
   print*, wname
   !call nor(dataS,NPTSS)
   call wsac(dataS,NPTSS,wname)              !生成新的SAC文件
   deallocate(dataS)
  else
   print*, 'The dir must be -,+ or a, please check!'
  endif
  deallocate(data)
 enddo

 99 continue
 deallocate(inver,dir,sacname)
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


subroutine wsac(data,n,wname)
 implicit none
 integer*4::i,n,len
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


subroutine nor(data,n)
 implicit none
 integer*4::n,i
 real*4::data(n),max
 max=0.0e0
 do i=1,n
  if(max.lt.abs(data(i))) max=abs(data(i))
 enddo
 data=data/max
 return
end subroutine
