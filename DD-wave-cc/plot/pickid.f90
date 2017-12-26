!This program is used to pick the relocated events in loc file, and write in a new file.
program pick
 implicit none
 integer*4::i,j,m,n,id,yr,mo,cid,nccp,nccs,nctp,ncts,id1,yr1,mo1,cid1
 real*4::sc,mag,rcc,rct,lat,lon,dep,x,y,z,ex,ey,ez,dy,hr,mi
 real*4::sc1,mag1,rcc1,rct1,lat1,lon1,dep1,x1,y1,z1,ex1,ey1,ez1,dy1,hr1,mi1
  
 open(10,file="hypoDD.loc")
 open(11,file="hypoDD.reloc")
 open(12,file="hypoDDloc.reloc")
 call filelen(10,m)
 call filelen(11,n)
 write(*,*) m,n
  
 do i=1,n
  read(11,*) id1!,lat1,lon1,dep1,x1,y1,z1,ex1,ey1,ez1,yr1,mo1,dy1,hr1,mi1,&
          !&  sc1,mag1,nccp,nccs,nctp,ncts,rcc,rct,cid1
  rewind(10)
  do j=1,m
   read(10,*) id,lat,lon,dep,x,y,z,ex,ey,ez,yr,mo,dy,hr,mi,sc,mag,cid
   if(id==id1) then
    write(12,*) id,lat,lon,dep,x,y,z,ex,ey,ez,yr,mo,dy,hr,mi,sc,mag,cid
    exit
   endif
  enddo
 enddo
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
