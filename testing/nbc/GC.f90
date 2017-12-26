!
program GreenC
 implicit none

 integer*4::i,j,k,num,IRU
 real*4::c,tc,tt,tcdif,ttdif,cdif,difc
 real*4,allocatable::dist(:)
 character*60::wname
 character*60,allocatable::name(:),namedif(:)

!读取控制文件信息
 open(9,file='gcfile',status='old')
 read(9,*) num
 allocate(dist(num),name(num),namedif(num))
 do i=1,num
  read(9,*) dist(i),name(i)
  read(9,*) namedif(i)
  print*, trim(name(i)),trim(namedif(i))
 enddo
 close(9)

 IRU=100
 do i=1,num
  wname=trim(namedif(i))//'.gc'
  open(IRU,file=name(i))
  open(IRU+1,file=namedif(i))
  open(IRU+2,file=wname)
  read(IRU,*)
  read(IRU+1,*)
  read(IRU,*)
  read(IRU+1,*)
  write(IRU+2,'(a,a,a,a,f8.2)') trim(name(i)),'  ',trim(namedif(i)),'   DIST=',dist(i)
  write(IRU+2,110)
  110 format(4x'Period'6x'difc'6x'cdif'3x'cdif-difc'5x'C'6x'C-difc')
  101 read(IRU,*,end=102) tc,tt
      read(IRU+1,*) tcdif,ttdif
      if(tc.ne.tcdif) then
       print*, 'The tc is not equal tcdif, please check!'
       goto 99
      endif
      c=dist(i)/tt
      cdif=dist(i)/(tt+tc/8)
      difc=dist(i)/(ttdif-tc/8)
      write(IRU+2,'(6f10.3)') tc,difc,cdif,cdif-difc,c,c-difc
  goto 101
  102 continue
  close(IRU)
  close(IRU+1)
  close(IRU+2)
 enddo

 99 continue
 deallocate(dist,name,namedif)
end program


