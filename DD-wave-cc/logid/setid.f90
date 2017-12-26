!功能：按照地震目录设定sac地震波性文件的id
!作者：王清东  时间：2015/8/25 10:33:25
program setid
 implicit none

 integer*4::i,j,k,m,nid,nerr,idd,num
 integer*4::year,month,day,hour,min,nsec,nmsec,days
 integer*4,allocatable::iyear(:),imonth(:),iday(:),idays(:),ihour(:),imin(:),id(:)
 real*4::tt
 real*4,allocatable::rsec(:),revla(:),revlo(:),rdepth(:),rmag(:)
 character::flag*1
 character::catalog*50,nmz*50,nmn*50,nme*50,nmr*50,nmt*50,cmd*100
 character,allocatable::sacname(:)*50

 !real*4,allocatable::data(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)


 open(10,file='setid.log')

 open(11,file='setid.in',status='old',action='read')
 read(11,*) catalog
 read(11,*) num
 allocate(sacname(num))
 do i=1,num
  read(11,*) sacname(i)
  call exist(sacname(i))
  sacname(i)=adjustl(sacname(i))
  print*, trim(sacname(i))
 enddo
 close(11)

 open(11,file=catalog,status='old',action='read')
 call filelen(11,m)
 allocate(iyear(m),imonth(m),iday(m),idays(m),ihour(m),imin(m),id(m))
 allocate(rsec(m),revla(m),revlo(m),rdepth(m),rmag(m))
 nid=0
 do i=1,m
  read(11,*,end=20) flag
  if(flag=='#') then  !找到事件标示
   nid=nid+1
   backspace(11)
   read(11,*) flag,iyear(nid),imonth(nid),iday(nid),ihour(nid),imin(nid),rsec(nid), &
              revla(nid),revlo(nid),rdepth(nid),rmag(nid),tt,tt,tt,id(nid)
   call date(iyear(nid),imonth(nid),iday(nid),idays(nid))
  endif
 enddo
 20 continue
 close(11)

 call system('mkdir nouse')
 do i=1,num
  j=index(sacname(i),'.Z.')
  if(j==0) then
   print*, 'The input is not Z component, please check!'
   stop
  endif
  nmz=sacname(i)(1:j)//'Z.SAC'
  nmn=sacname(i)(1:j)//'N.SAC'
  nme=sacname(i)(1:j)//'E.SAC'
  nmr=sacname(i)(1:j)//'R.SAC'
  nmt=sacname(i)(1:j)//'T.SAC'
  call brsach(100,nmz,nerr)
  year=ihdr(1)
  days=ihdr(2)
  hour=ihdr(3)
  min=ihdr(4)
  nsec=ihdr(5)
  nmsec=ihdr(6)  !将发震时刻设为参考时间
  idd=-1
  do j=1,nid
   if(year==iyear(j).and.days==idays(j).and.hour==ihour(j).and.min==imin(j)  &
      .and.nsec==floor(rsec(j)).and.nmsec==nint((rsec(j)-floor(rsec(j)))*1000)) then
    idd=id(j)
    exit
   endif
  enddo
  if(idd/=-1.and.idd/=ihdr(13)) then
   ihdr(13)=idd
   call bwsach(100,nmz,nerr)
   call brsach(100,nmn,nerr)
   ihdr(13)=idd
   call bwsach(100,nmn,nerr)
   call brsach(100,nme,nerr)
   ihdr(13)=idd
   call bwsach(100,nme,nerr)
   call brsach(100,nmr,nerr)
   ihdr(13)=idd
   call bwsach(100,nmr,nerr)
   call brsach(100,nmt,nerr)
   ihdr(13)=idd
   call bwsach(100,nmt,nerr)
  elseif(idd==-1) then
   cmd='mv '//trim(nmz)//' nouse/'
   print*, trim(cmd)
   call system(trim(cmd))
   cmd='mv '//trim(nmn)//' nouse/'
   print*, trim(cmd)
   call system(trim(cmd))
   cmd='mv '//trim(nme)//' nouse/'
   print*, trim(cmd)
   call system(trim(cmd))
   cmd='mv '//trim(nmr)//' nouse/'
   print*, trim(cmd)
   call system(trim(cmd))
   cmd='mv '//trim(nmt)//' nouse/'
   print*, trim(cmd)
   call system(trim(cmd))
  endif
 enddo


 99 continue
end program


subroutine filelen(fileid,n)
!得到文件行数
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


subroutine date(year,month,day,days)
!将日期转化为一年中的第几天;即SAC中的NZJDAY
 implicit none
 integer*4::i,year,month,day,days
 integer*4::mon(12)
 data mon /31,28,31,30,31,30,31,31,30,31,30,31/
 !判断是否闰年:能被4整除但不能被100整除以及能被400整除的年份为闰年。
 if(mod(year,4)==0.and.mod(year,100)/=0) then
  mon(2)=29
 elseif(mod(year,400)==0) then
  mon(2)=29
 endif
 days=day
 do i=1,month-1
  days=days+mon(i)
 enddo
 return
end subroutine


subroutine bwsach(IRU,name,nerr)
!-----
!       IRU I*4 logical unit for IO
!       name    C*  Name of file to be opened
!       rhdr    R*4 Real header
!       ihdr    I*4 Integer Header
!       chdr    C*  Character Header
!       nerr    I*4 -1 file does not exist
!               -2 data points in file exceed dimension
!-----
!  This routine reads waveform data written in SAC binary format.
!
!  Written by WQD, 2013/8/24 10:49:39
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
 write(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40),(chdr(i),i=1,24)
 close(IRU)
 return
end subroutine

