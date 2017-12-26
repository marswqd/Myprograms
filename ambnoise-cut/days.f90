!本程序的功能是: 连接sac文件，截取其中nday的数据生成n个新的一天的sac文件，并计算数据的zerogap
!作者: 王清东  时间: 2011-09-20 14:31:40
program days
 implicit none
 
 integer*4::i,j,k,IRU,nerr,num,year,day,nday
 integer*4::INPTS,head,tail,ihead,itail,dhead,dtail
 integer*4,parameter::daysec=86400              
 integer*4,allocatable::NPTS(:)
 character*60::wname
 character*60,allocatable::sacname(:)
 real*4::gap
 real*8::t,ba,ea                          !为保持精度,防止大数吃小数,本程序中的时间变量都定义为双精度
 real*8,parameter::DT=1.0d0               !DT:原始数据采样间隔
 real*4,allocatable::idata(:)
 real*8,allocatable::B(:),E(:)
 
 real*4,allocatable::data(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)

 open(10,file='info.txt')                 !程序运行信息文件

!读取控制文件信息 
 open(11,file='daysfile',status='old')
 read(11,*) year,day,nday                 !设定时间基准,和生成文件包含的天数
 read(11,*) num                           !num:处理的sac文件数
 allocate(sacname(num))                   !sacname:sac文件名
 do i=1,num
  read(11,*) sacname(i)
  call filesta(sacname(i),nerr)
  if(nerr.eq.1) then
   print*, 'The file ',trim(sacname(i)),' dosen`t exist'
   goto 99
  endif
  sacname(i)=adjustl(sacname(i))
  !print*, trim(sacname(i))
 enddo
 close(11)
 
!读SAC文件头，提取必要信息 
 allocate(NPTS(num),B(num),E(num))                      !NPTS,B,E:sac文件头变量
 IRU=100
 write(10,*) 'SACNAME  B  NPTS  E'
 do i=1,num
  call brsach(IRU,sacname(i),nerr)
! DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
! NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
! NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)
  !print*, 'DT=',rhdr(1)
  !DT=rhdr(1)*1.0d0
  NPTS(i)=ihdr(10)
  t=((((ihdr(1)-year)*365.0d0+ihdr(2)-day)*24.0d0+ihdr(3))*60.0d0+   &
        ihdr(4))*60.0d0+ihdr(5)*1.0d0+ihdr(6)*0.001d0  
  B(i)=t+rhdr(6)                          !t:换算参考时间为基准时间,单位s
  E(i)=t+rhdr(7)
  write(10,*) trim(sacname(i)),'   ',B(i),NPTS(i),E(i)
 enddo
 write(10,*) '------------------------------------------------------------------'

!读取波形数据,按时间顺序排列(idata)
 ba=minval(B)
 ea=maxval(E)
 INPTS=nday*daysec
 !print*, 'INPTS= ',INPTS
 allocate(idata(INPTS))
 idata=0.0e0
 if(ba.gt.0.0d0) then
  write(10,*) 'The begin time of the data is after that we want, maybe bad'
  write(10,*) '-----------------------------------------------------------------'
  print*, 'The begin time of the data is after that we want, maybe bad'
 endif
 if(ea.lt.INPTS*DT) then
  write(10,*) 'The end time of the data is before that we want, maybe bad'
  write(10,*) '-----------------------------------------------------------------'
  print*, 'The end time of the data is before that we want, maybe bad'
 endif
 do i=1,num
  !print*, 'NPTS(i)=',NPTS(i)
  allocate(data(NPTS(i)))
  call brsac(IRU,NPTS(i),sacname(i),data,nerr)
  head=anint(B(i)/DT)+1
  tail=anint(E(i)/DT)+1
  if(head.ge.1.and.tail.le.INPTS) then
   ihead=head
   itail=tail
   dhead=1
   dtail=NPTS(i)
  else if(head.lt.1.and.tail.le.INPTS) then
   ihead=1
   itail=tail
   dhead=-head+2
   dtail=NPTS(i)
  else if(head.ge.1.and.tail.gt.INPTS) then
   ihead=head
   itail=INPTS
   dhead=1
   dtail=dhead+(itail-ihead)
  else
   ihead=1
   itail=INPTS
   dhead=-head+2
   dtail=dhead+(itail-ihead)
  endif
  idata(ihead:itail)=data(dhead:dtail)
  deallocate(data)
 enddo
 
!去时间标识(缺)

!计算一天数据中零值占得比重,作为剔除数据的标准
 k=0
 allocate(data(daysec))
 write(10,*) 'daygap < 0.2 is a good gap'
 do i=1,nday
  ihdr(2)=day+i-1 
  j=(i-1)*daysec
  data(1:daysec)=idata(j+1:j+daysec)
  call zerogap(data,daysec,gap)
  if(gap.gt.0.2e0) then
   k=k+1
   write(10,*) ihdr(2),gap ,' The gap is too large'
   print*, 'The gap of day ',ihdr(2),' is too large'
  else
   write(10,*) ihdr(2),gap
  endif 
 enddo
 deallocate(data)
 if(k.ne.0) then
  write(10,*) 'gapdays = ',k
 endif
 call zerogap(idata,INPTS,gap)
 if(gap.gt.0.2e0) then
  write(10,*) 'gap= ',gap,' The station gap is too large, give up'
  print*, 'gap= ',gap,' The station gap is too large, give up'
  open(12,file='giveupthestation')
  close(12)
  goto 99        !数据缺失过多,舍弃这一台站,结束程序
 else
  write(10,*) 'gap= ',gap
  print*, 'gap= ',gap
 endif
 write(10,*) '-----------------------------------------------------------------'

!生成一天的sac文件(生成的sac文件头以第一个文件为标准)
 allocate(data(daysec))
 write(10,*) 'New sac name'
 call brsach(101,sacname(1),nerr)
 ihdr(1)=year
 ihdr(3)=00
 ihdr(4)=00
 ihdr(5)=00
 ihdr(6)=000
 rhdr(1)=DT
 rhdr(6)=0.0e0
 rhdr(7)=(daysec-1)*rhdr(1)
 do i=1,nday
  ihdr(2)=day+i-1 
  j=(i-1)*daysec
  data(1:daysec)=idata(j+1:j+daysec)
! NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
! NZSEC=ihdr(5)  NZMSEC=ihdr(6)
  wname(1:4)=chdr(1)
  wname(5:5)='.'
  write(wname(6:9),'(i4)') ihdr(1)
  if(ihdr(2).lt.10) then
   wname(10:11)='00'
   write(wname(12:12),'(i1)') ihdr(2)
  elseif(ihdr(2).lt.100) then
   wname(10:10)='0'
   write(wname(11:12),'(i2)') ihdr(2)
  else
   write(wname(10:12),'(i3)') ihdr(2)
  endif
  wname(13:16)='.SAC'
  write(10,*) trim(wname)
  print*, trim(wname)
  call wsac(data,daysec,wname)              !生成新的SAC文件
 enddo
 deallocate(data,idata)
 write(10,*) '-----------------------------------------------------------------'

 99 continue
 deallocate(E,B,NPTS)
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


subroutine zerogap(data,n,gap)
 implicit none
 integer*4::i,j,n   
 real*4::data(n),gap
 j=0
 do i=1,n
  if(data(i).eq.0.0e0) j=j+1
 enddo
 gap=(j*1.0e0)/(n*1.0e0)
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
