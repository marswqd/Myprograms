!功能：按地震事件id挑选符合要求的的sac地震波性文件
!作者：王清东  时间：2015/8/24 14:55:10
program selid
 implicit none

 integer*4::i,j,k,m,n,num,idd,nid,nerr
 integer*4,allocatable::id(:),idn(:)
 real*4::rtmp
 character::flag*1,phase*1,tempc2*1,tempc1*5
 character::catalog*50,nmz*50,nmn*50,nme*50,nmr*50,nmt*50,cmd*100
 character,allocatable::sacname(:)*50

 !real*4,allocatable::data(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)


 open(10,file='selid.log')

 open(11,file='selid.in')
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
 allocate(id(m),idn(m))
 nid=0
 do i=1,m
  read(11,*,end=20) flag
  if(flag=='#') then  !找到事件标示
   nid=nid+1
   backspace(11)
   read(11,*) flag,j,j,j,j,j,rtmp,rtmp,rtmp,rtmp,rtmp,rtmp,rtmp,rtmp,id(nid)
  endif
 enddo
 20 continue
 close(11)

 idn=0
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
  idd=ihdr(13)
  k=0
  do j=1,nid
   if(idd==id(j)) then
    idn(j)=idn(j)+1
    k=1
    exit
   endif
  enddo
  if(k==0) then
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

 do i=1,nid
  write(10,*) id(i),idn(i)
  if(idn(i)==0) print*, 'No wave file for event: ',id(i)
 enddo


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

