!-----------------------------------------------------------------------------!
!
! Author: Wang Qingdong, School of Geodesy and Geomatics, Wuhan University,
!                        Wuhan City, Hubei Province, China. 430079.
! E-mail: wqd1986@whu.edu.cn
! Time:   2014/5/30 19:46:06
! Function: get station location (latitude, longitude, elevation) for the SAC files
!           The SAC file's name is just like 20120907084203.Kun.HWS.BHE
!
!-----------------------------------------------------------------------------!
program getsta
 implicit none

 integer*4::i,j,m,n,nerr,num,nsta
 real*4::T1,T2
 character::tempc1*5,name*60
 character,allocatable::sta(:)*5,sacname(:)*60

 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)


 call CPU_TIME(T1)
 !open(10,file='picke.log')

!read control file
 open(11,file='getsta.in',status='old')
 call filelen(11,num)
 read(11,*) num
 allocate(sacname(num),sta(num))
 do i=1,num
  read(11,*) sacname(i)
  call exist(sacname(i))
  sacname(i)=adjustl(sacname(i))
  print*, trim(sacname(i))
 enddo
 close(11)

 !search station name
! DT=rhdr(1)     B=rhdr(6)      O=rhdr(8)      DIST=rhdr(51)
! A=rhdr(9)      T0=rhdr(11)    T1=rhdr(12)    T2=rhdr(13)
! STLA=rhdr(32)  STLO=rhdr(33)  STEL=rhdr(34)  STDP=rhdr(35)
! EVLA=rhdr(36)  EVLO=rhdr(37)  EVEL=rhdr(38)  EVDP=rhdr(39)
! NZYEAR=ihdr(1) NZJDAY=ihdr(2) NZHOUR=ihdr(3) NZMIN=ihdr(4)
! NZSEC=ihdr(5)  NZMSEC=ihdr(6) NPTS=ihdr(10)  KSTNM=chdr(1)
 open(12,file='station.txt')
 nsta=1
 name=sacname(1)
 !call dot(name,2,m)
 !call dot(name,3,n)
 !sta(1)=name(m+1:n-1)
 call brsach(100,name,nerr)
 sta(1)=chdr(1)
 call isnul(sta(1))
 call upper(sta(1))
 write(12,*) trim(sta(1)),rhdr(32),rhdr(33),rhdr(34) !station latitude longitude elevation
 i=2
 20 do while(i<=num)
  name=sacname(i)
  i=i+1
  !call dot(name,2,m)
  !call dot(name,3,n)
  !tempc1=name(m+1:n-1)
  call brsach(100,name,nerr)
  tempc1=chdr(1)
  call isnul(tempc1)
  call upper(tempc1)
  do j=1,nsta
   if(tempc1==sta(j)) goto 20
  enddo
  nsta=nsta+1
  sta(nsta)=tempc1
  !call upper(sta(nsta))
  !call brsach(100,name,nerr)
  write(12,*) trim(sta(nsta)),rhdr(32),rhdr(33),rhdr(34)
 enddo
 close(12)
 print*, 'There are ',nsta,' stations in the SAC files, see the file station.txt.'


 99 continue
 call CPU_TIME(T2)
 print*, 'T=',T2-T1
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


subroutine filelen(fileid,n)
!Get the number of lines of the file
 implicit none
 integer*4::fileid,n
 rewind(fileid)
 n=0
 do
  read(fileid,*,end=20)
  n=n+1
 enddo
 20 rewind(fileid)
 return
end subroutine


subroutine dot(str,n,i)
!寻找字符串str中的第n个.号的位置i
 implicit none
 integer*4::i,j,k,n
 character*1::flag
 character*(*)::str
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


subroutine isnul(string)
 !字符串中nul变为空格
 implicit none
 integer*4::i,strlen
 character*(*)::string
 strlen=len_trim(string)
 do i=1,strlen
  if(ichar(string(i:i))==0) then
   string(i:i)=' '  !ichar('NUL')=0  ichar(' ')=32
  endif
 enddo
 return
end subroutine


subroutine lower(string)
 !字符串大写变小写
 implicit none
 integer*4::i,strlen
 character*(*)::string
 strlen=len_trim(string)
 do i=1,strlen
  if(string(i:i)>='A'.and.string(i:i)<='Z') then
   string(i:i)=char(ichar(string(i:i))+ichar('a')-ichar('A'))
  endif
 enddo
 return
end subroutine


subroutine upper(string)
 !字符串小写变大写
 implicit none
 integer*4::i,strlen
 character*(*)::string
 strlen=len_trim(string)
 do i=1,strlen
  if(string(i:i)>='a'.and.string(i:i)<='z') then
   string(i:i)=char(ichar(string(i:i))-ichar('a')+ichar('A'))
  endif
 enddo
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


