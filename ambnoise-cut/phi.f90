program phi
 integer*4::i,j,num
 real*4::t(46),ap(46),a(46),dt,p
 real*4,parameter::pi=3.14159265358e0
 character*60::nameap,flag

 open(9,file='phi.txt')
 a=0.0e0
 open(10,file='phifile',status='old')
 do i=1,3
  read(10,*) nameap
  open(11,file=nameap,status='old')
  do j=1,46
   read(11,*) t(j),ap(j)
   a(j)=a(j)+ap(j)
  enddo
  close(11)
 enddo
 a=a/maxval(a)
 read(10,*) nameap
 close(10)
 open(11,file=nameap,status='old')
 do i=1,3
  read(11,'(a60)') flag
  write(9,*) flag
 enddo
 write(9,110)
 110 format('Period'6x'dt'7x'A'3x'PHI/(-pi/4)')
 read(11,*)
 do i=1,46
  read(11,*) t(i),dt,dt,dt,dt,p
  write(9,'(f6.2,3f9.3)') t(i),dt,a(i),p
 enddo
 close(9)

end program



