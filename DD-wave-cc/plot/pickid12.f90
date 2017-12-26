!This program is used to pick the relocated events in loc file, and write in a new file.
program pick
 implicit none
 integer*4::i,j,k,m,n,id2,yr,mo,cid,nccp,nccs,nctp,ncts,id1,yr1,mo1,cid1
 real*4::sc,mag,rcc,rct,lat,lon,dep,x,y,z,ex,ey,ez,dy,hr,mi
 real*4::sc1,mag1,rcc1,rct1,lat1,lon1,dep1,x1,y1,z1,ex1,ey1,ez1,dy1,hr1,mi1
 character::card1*200,card2*200
  
 open(10,file="../hypoDD.reloc")
 open(11,file="hypoDD.reloc")
 open(12,file="1hypoDD.reloc")
 open(13,file="2hypoDD.reloc")
 call filelen(10,m)
 call filelen(11,n)
 write(*,*) m,n
  
 k=0
 do i=1,n
  read(11,'(a200)') card1
  read(card1,*) id1
  rewind(10)
  do j=1,m
   read(10,'(a200)') card2
   read(card2,*) id2
   if(id2==id1) then
    k=k+1
    write(12,'(a200)') card2
    write(13,'(a200)') card1
    exit
   endif
  enddo
 enddo
 print*, k
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
