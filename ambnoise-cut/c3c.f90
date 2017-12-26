program c3c
 implicit none

 integer*4::i,j,k,num,IRU
 real*4::tc,tt,c12,c23,c13,cc12,cc23,cc13,AGC12,AGC23,AGC13
 !real*4::dc12,dc23,dc13
 !real*4,allocatable::dist(:)
 character*60::wname,flag,AG12,AG23,AG13,n3c
 !character*60,allocatable::n3c(:)

!读取控制文件信息
 open(9,file='c3cfile',status='old')
 read(9,*) AG12
 read(9,*) AG23
 read(9,*) AG13
 read(9,*) num
 open(10,file=AG12)
 open(11,file=AG23)
 open(12,file=AG13)

 do i=1,num
  read(9,*) n3c,wname
  wname=trim(wname)//'.CC.txt'
  print*, trim(wname)
  open(13,file=n3c)
  open(14,file=wname)
  do j=1,3
   read(13,'(a60)') flag
   write(14,*) flag
  enddo
  read(13,*)
  write(14,120)
  do j=1,2
   read(10,*)
   read(11,*)
   read(12,*)
  enddo
  120 format('Period'5x'c12'5x'cc12'5x'AGC12'1x'c12-AGC12'1x'cc12-AGC12'   &
             2x'c23'5x'cc23'5x'AGC23'1x'c23-AGC23'1x'cc23-AGC23'           &
             2x'c13'5x'cc13'5x'AGC13'1x'c13-AGC13'1x'cc13-AGC13')
  101 read(13,*,end=102) tc,tt,tt,tt,tt,tt,c12,cc12,c23,cc23,c13,cc13
   read(10,*) tc,AGC12,AGC12,AGC12,AGC12,AGC12,AGC12
   read(11,*) tc,AGC23,AGC23,AGC23,AGC23,AGC23,AGC23
   read(12,*) tc,AGC13,AGC13,AGC13,AGC13,AGC13,AGC13
   write(14,'(f6.2,15f9.4)') tc,c12,cc12,AGC12,c12-AGC12,cc12-AGC12,c23,cc23,AGC23,  &
                             c23-AGC23,cc23-AGC23,c13,cc13,AGC13,c13-AGC13,cc13-AGC13
   goto 101
  102 continue
  close(13)
  close(14)
  rewind(10)
  rewind(11)
  rewind(12)
 enddo
 close(9)
 close(10)
 close(11)
 close(12)
 99 continue
 !deallocate(dist,name,namedif)
end program


