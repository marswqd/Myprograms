!功能: 地震背景噪声三台法的相速度修正(amend)
!作者: 王清东  时间: 2011-06-08 21:10:50
program n3gc
 implicit none

 integer*4::i,j,num,ntc,IRU
 real*4::tc(3),t,tt12,tt23,tt13,c12,c23,c13,d12,d23,d13
 real*4::cc12,cc23,cc13,dd,dtt,phi
 real*4,parameter::pi=3.14159265358e0
 character*60::wname,name12,name23,name13,flag

!读取控制文件信息
 IRU=100
 open(9,file='n3cfile',status='old')
 read(9,*) num                            !num:处理的实际文件数为num*3
 do i=1,num
  read(9,*) name12
  read(9,*) name23
  read(9,*) name13
  read(9,*) wname
  wname=trim(wname)//'.3GC.txt'
  print*, trim(wname)
  open(IRU,file=wname)
  open(IRU+1,file=name12,status='old')
  open(IRU+2,file=name23,status='old')
  open(IRU+3,file=name13,status='old')
  read(IRU+1,*) flag,flag,d12
  read(IRU+2,*) flag,flag,d23
  read(IRU+3,*) flag,flag,d13
  dd=d12+d23-d13
  read(IRU+1,*)
  read(IRU+2,*)
  read(IRU+3,*)
  write(IRU,'(a,a,f7.2)') trim(name12),'  DIST12=',d12
  write(IRU,'(a,a,f7.2)') trim(name23),'  DIST23=',d23
  write(IRU,'(a,a,f7.2,a,f5.2)') trim(name13),'  DIST13=',d13,'  dd=',dd
  write(IRU,110)
  110 format('Period'5x'TTGC12'3x'TTGC23'2x'TTGC13'5x'dTTGC'1x'PHI/(-pi/4)'    &
             1x'GC12'5x'GCC12'4x'GC23'5x'GCC23'4x'GC13'5x'GCC13'1x'GCC12-GC12' &
             1x'GCC23-GC23'1x'GCC13-GC13')
  120 read(IRU+1,*,end=130) tc(1),tt12,tt12,tt12,tt12,tt12
   read(IRU+2,*) tc(2),tt23,tt23,tt23,tt23,tt23
   read(IRU+3,*) tc(3),tt13,tt13,tt13,tt13,tt13
   if(tc(3).ne.tc(2).or.tc(1).ne.tc(2)) then
    print*, 'The tc is not equal, please check!'
    goto 99
   endif
   t=tc(3)
   c12=d12/(tt12-t/8)
   c23=d23/(tt23-t/8)
   c13=d13/(tt13-t/8)
   dtt=tt12+tt23-tt13
   phi=2*pi/t*(dtt-2*dd/(c12+c23))
   cc12=d12/(tt12-t/(2*pi)*phi)
   cc23=d23/(tt23-t/(2*pi)*phi)
   cc13=d13/(tt13-t/(2*pi)*phi)
   write(IRU,'(f6.2,2x,5f9.3,9f9.4)') t,tt12,tt23,tt13,dtt,-phi/pi*4,c12,cc12,c23,cc23,c13,cc13,cc12-c12,cc23-c23,cc13-c13
  goto 120
  130 continue
  close(IRU+1)
  close(IRU+2)
  close(IRU+3)
 enddo

 99 continue
 close(9)
 close(IRU)
end program








