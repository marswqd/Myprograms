!本程序的功能是:读已经过定位和震相标定的地震事件SAC文件,生成hypoDD型地震目录.
!  注: 本程序读取同一分量的SAC文件,文件命名方式为 P.BAS.20120907111943.E.SAC
!作者: 王清东  时间: 2013/9/27 10:48:12
program catdd
 implicit none
 
 integer*4::i,j,k,m,n,nerr,num,nsta,npos,neve,ID
 integer*4::year,month,day,hour,min,nsec,nmsec,days
 integer*4,allocatable::pos(:)
 real*4::T1,T2,sec,evla,evlo,depth,mag,tt,DIST,AZ,Pweight,Sweight,O,A,T0
 character*5::tempc1
 character*16::tempc2
 character*60::name
 character*5,allocatable::sta(:)
 character*16,allocatable::eve(:)
 character*60,allocatable::sacname(:)
 
 real*4,allocatable::data(:),adata(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)


 call CPU_TIME(T1)

 open(10,file='catdd.log')
!震相权重
 Pweight=1.0e0
 Sweight=0.5e0

!读取控制文件信息 
 open(11,file='catdd.in',status='old')
 read(11,*) num
 allocate(sacname(num),sta(num),eve(num),pos(num))
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

 print*, 'Step 1'
 !寻找台站,生成台站信息文件
 open(12,file='station.txt')
 nsta=1
 name=sacname(1)    !从sac文件名中得到台站名
 !call dot(name,1,m)
 !call dot(name,2,n) !用于形如P.BAS.20120907111943.E.SAC的文件
 call dot(name,0,m)
 call dot(name,1,n)  !用于形如BAS.20120907111943.E.SAC的文件
 sta(1)=name(m+1:n-1)
 call brsach(100,name,nerr)
 write(12,*) sta(1),rhdr(32),rhdr(33)
 i=2
 20 do while(i<=num)
  name=sacname(i)
  i=i+1
  !call dot(name,1,m)
  !call dot(name,2,n) !用于形如P.BAS.20120907111943.E.SAC的文件
  call dot(name,0,m)
  call dot(name,1,n)  !用于形如BAS.20120907111943.E.SAC的文件
  tempc1=name(m+1:n-1)
  do j=1,nsta
   if(tempc1==sta(j)) goto 20
  enddo
  nsta=nsta+1
  sta(nsta)=name(m+1:n-1)
  call brsach(100,name,nerr)
  write(12,*) sta(nsta),rhdr(32),rhdr(33)
 enddo
 close(12)
 write(10,*) 'There are ',nsta,' stations in the SAC files, see the file station.txt.'

 print*, 'Step 2'
 !寻找事件.分量数
 neve=1
 name=sacname(1)
 !call dot(name,2,m)
 !call dot(name,4,n)  !用于形如P.BAS.20120907111943.E.SAC的文件
 call dot(name,1,m)
 call dot(name,3,n)   !用于形如BAS.20120907111943.E.SAC的文件
 eve(1)=name(m+1:n-1)
 i=2
 22 do while(i<=num)
  name=sacname(i)
  i=i+1
  !call dot(name,2,m)
  !call dot(name,4,n)  !用于形如P.BAS.20120907111943.E.SAC的文件
  call dot(name,1,m)
  call dot(name,3,n)   !用于形如BAS.20120907111943.E.SAC的文件
  tempc2=name(m+1:n-1)
  do j=1,neve
   if(tempc2==eve(j)) goto 22
  enddo
  neve=neve+1
  eve(neve)=name(m+1:n-1)
 enddo
 write(10,*) 'There are ',neve,' event.component in the SAC files, see the file phase.txt.'
 write(10,*) '-----'

 print*, 'Step 3'
 !寻找具有相同事件.分量的SAC文件在文件列表中的位置
 open(13,file='catdd.txt')
 do i=1,neve
  npos=0
  do j=1,num
   name=sacname(j)
   !call dot(name,2,m)
   !call dot(name,4,n)  !用于形如P.BAS.20120907111943.E.SAC的文件
   call dot(name,1,m)
   call dot(name,3,n)   !用于形如BAS.20120907111943.E.SAC的文件
   tempc2=name(m+1:n-1)
   if(tempc2==eve(i)) then
    npos=npos+1
    pos(npos)=j
   endif
  enddo
  !读相同事件.分量的sac头文件
  k=pos(1)
  call brsach(100,sacname(k),nerr)
  year=ihdr(1)
  days=ihdr(2)
  hour=ihdr(3)
  min=ihdr(4)
  nsec=ihdr(5)
  nmsec=ihdr(6)
  evla=rhdr(36)  !事件纬度
  evlo=rhdr(37)  !事件经度
  depth=rhdr(39) !震源深度EVDP
  mag=rhdr(40)   !震级
  tt=0.0e0
  ID=ihdr(13)    !NXSIZE,设为事件ID
  call andate(year,days,month,day)
  sec=nmsec*1.0e0/1000+nsec*1.0e0
  write(13,110) '#',year,month,day,hour,min,sec,evla,evlo,depth,mag,tt,tt,tt,ID
  110 format(a1,2x,i4,2x,i2,2x,i2,2x,i2,2x,i2,2x,f5.2,2x,f7.3,2x,f7.3,2x,f7.2,2x,f3.1,2x,3f4.1,i8)
  name=sacname(k)
  !call dot(name,1,m)
  !call dot(name,2,n) !用于形如P.BAS.20120907111943.E.SAC的文件
  call dot(name,0,m)
  call dot(name,1,n)  !用于形如BAS.20120907111943.E.SAC的文件
  tempc1=name(m+1:n-1)
  O=rhdr(8)
  if(O/=0.0e0) print*, 'The O is not zore, please check!'
  A=rhdr(9)
  T0=rhdr(11)
  DIST=rhdr(51)
  AZ=rhdr(52)
  if(A/=-12345.0e0) then
   write(13,'(a,2x,f7.2,2x,f3.1,2x,a1,2x,f8.3,2x,f6.2)') tempc1,A,Pweight,'P',DIST,AZ
  endif
  if(T0/=-12345.0e0) then
   write(13,'(a,2x,f7.2,2x,f3.1,2x,a1,2x,f8.3,2x,f6.2)') tempc1,T0,Sweight,'S',DIST,AZ
  endif
  print*, 'Beign with: ',eve(i)
  write(10,*) 'Beign with: ',eve(i),evla,evlo,depth,mag,ID
  print*, sacname(k)
  write(10,*) trim(sacname(k)),rhdr(32),rhdr(33),DIST,AZ
  do j=2,npos
   k=pos(j)
! DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
! A=rhdr(9)      T0=rhdr(11)    T1=rhdr(12)    T2=rhdr(13)
! STLA=rhdr(32)  STLO=rhdr(33)  STEL=rhdr(34)  STDP=rhdr(35)
! EVLA=rhdr(36)  EVLO=rhdr(37)  EVEL=rhdr(38)  EVDP=rhdr(39)
! NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
! NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)  KSTNM=chdr(1)
   call brsach(100,sacname(k),nerr)
   name=sacname(k)
   !call dot(name,1,m)
   !call dot(name,2,n) !用于形如P.BAS.20120907111943.E.SAC的文件
   call dot(name,0,m)
   call dot(name,1,n)  !用于形如BAS.20120907111943.E.SAC的文件
   tempc1=name(m+1:n-1)
   O=rhdr(8)
   A=rhdr(9)
   T0=rhdr(11)
   DIST=rhdr(51)
   AZ=rhdr(52)
   if(A/=-12345.0e0) then
    write(13,'(a,2x,f7.2,2x,f3.1,2x,a1,2x,f8.3,2x,f6.2)') tempc1,A,Pweight,'P',DIST,AZ
   endif
   if(T0/=-12345.0e0) then
    write(13,'(a,2x,f7.2,2x,f3.1,2x,a1,2x,f8.3,2x,f6.2)') tempc1,T0,Sweight,'S',DIST,AZ
   endif
   print*, sacname(k)
   write(10,*) trim(sacname(k)),rhdr(32),rhdr(33),DIST,AZ
  enddo
 enddo
 close(13)


 99 continue
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
!  Written by Hafidh A. A. Ghalib, 1988.
!
!  Modified by Wang, 2012
!-----
 implicit none
 integer*4::i,IRU,LN,nerr,maxpts,nbytes
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
  nerr=-2
 else 
  maxpts=ihdr(10)
  nerr=0
 endif
 nbytes=632+4*maxpts
 open(IRU,file=name,form='unformatted',access='direct',recl=nbytes)
 read(IRU,rec=1)  (rhdr(i),i=1,70),   &
                  (ihdr(i),i=1,40),   &
                  (chdr(i),i=1,24),   &
                  (data(i),i=1,maxpts)
 close(IRU)
 ihdr(10)=maxpts
 return
end subroutine


subroutine dot(str,n,i)
!寻找字符串str中的第n个.号的位置i
 implicit none
 integer*4::i,j,k,n
 character*1::flag
 character*(*)::str
 if(n==0) then
  i=0
  return
 endif
 j=len_trim(str)
 k=0
 do i=1,j
  flag=str(i:i)
  if(flag=='.') then
   k=k+1
   if(k==n) exit
  endif
 enddo
 if(k==0) print*, 'No dot in:',str
 return
end subroutine


subroutine andate(year,days,month,day)
!将一年中的第几天(SAC中的NZJDAY)转化为年月日
 implicit none
 integer*4::i,year,month,day,days
 integer*4::mon(12)
 data mon /31,28,31,30,31,30,31,31,30,31,30,31/
 if(mod(year,4).eq.0) mon(2)=29  !判断是否为闰年
 day=days
 do i=1,12
  if(day<mon(i)) then
   month=i
   exit
  endif
  day=day-mon(i)
 enddo
 return
end subroutine

