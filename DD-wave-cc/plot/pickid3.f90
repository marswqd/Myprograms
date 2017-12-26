!This program is used to pick the relocated events in loc file, and write in a new file.
program pickid3
 implicit none
 integer*4::i,j,k,m,n,l,a,b,c,idct,idcc,idctcc
 !real*4::sc,mag,rcc,rct,lat,lon,dep,x,y,z,ex,ey,ez,dy,hr,mi
 !real*4::sc1,mag1,rcc1,rct1,lat1,lon1,dep1,x1,y1,z1,ex1,ey1,ez1,dy1,hr1,mi1
 character*200::linect,linecc,linectcc

!input
 open(10,file="hypoDD-ct.reloc")
 open(11,file="hypoDD-cc.reloc")
 open(12,file="hypoDD-ctcc.reloc")
!output
 open(13,file="ct.reloc")
 open(14,file="cc.reloc")
 open(15,file="ctcc.reloc")

 call filelen(10,m)
 call filelen(11,n)
 call filelen(12,l)
 c=min(m,n,l)
 write(*,*) m,n,l,c

 b=0
 rewind(10)
 do i=1,m
  a=0
  read(10,'(a200)') linect
  read(linect,*) idct
  rewind(11)
  loop1: do j=1,n
   read(11,'(a200)') linecc
   read(linecc,*) idcc
   rewind(12)
   loop2: do k=1,l
    read(12,'(a200)') linectcc
    read(linectcc,*) idctcc
    if(idct==idctcc.and.idcc==idctcc) then
     write(13,*) trim(linect)
     write(14,*) trim(linecc)
     write(15,*) trim(linectcc)
     print*, 'ID:', idctcc
     b=b+1
     a=1
     exit loop1
    endif
   enddo loop2
   if(a==1) exit
  enddo loop1
  if(b==c) exit
 enddo
 print*, b
end program

subroutine filelen(fileid,n)
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
