! Statistics seismic phase azimuth distribution
program azn
 implicit none

 integer*4::i,j,m,numP(360),numS(360)
 real*4::tmp,azl,azr,az1,az2
 character::str1*1,str5*5

 open(10,file='cc-s.txt')
 call exist('cc-s.txt')
 open(11,file='azn.txt')

 numP=0
 numS=0
 do
  read(10,*,end=20) m,m,tmp,str1,str5,tmp,tmp,tmp,tmp,tmp,az1,az2
  j=0
  do i=1,360
   azl=(i-1)*1.0e0
   azr=i*1.0e0
   if(str1=='P') then
    if(az1<azr.and.az1>=azl) then
     numP(i)=numP(i)+1
     j=j+1
    endif
    if(az2<azr.and.az2>=azl) then
     numP(i)=numP(i)+1
     j=j+1
    endif
   else
    if(az1<azr.and.az1>=azl) then
     numS(i)=numS(i)+1
     j=j+1
    endif
    if(az2<azr.and.az2>=azl) then
     numS(i)=numS(i)+1
     j=j+1
    endif
   endif
   if(j==2) exit
  enddo
 enddo
 20 continue
 do i=1,360
  write(11,*) i,numP(i),numS(i)
 enddo

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

