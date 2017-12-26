program narrowbandfilter
 implicit none
 integer::i
 real*8::B,DT,T0,T1,T2,T3,T4,t,NPTS,n,m
 real*8,allocatable::NB(:)
 real*8,parameter::pi=3.14159265357D0

 open(10,file='sac-data',status='old')
 read(10,*) B
 read(10,*) NPTS
 read(10,*) DT
 read(10,*) T0
 read(10,*) n
 read(10,*) m
 close(10)

 write(*,*) B,NPTS,DT,T0,n,m

 allocate(NB(int(NPTS)))
 T1=T0-(m+n)*DT
 T2=T0-n*DT
 T3=T0+n*DT
 T4=T0+(m+n)*DT

 do i=1,int(NPTS)
   t=B+(i-1)*DT
   if(t.GE.T1.AND.t.LE.T2) then
     NB(i)=0.5D0*(cos(pi*(t-T2)/(T2-T1))+1.0D0)
   else if(t.GT.T2.AND.t.LT.T3) then
     NB(i)=1.0
   else if(t.GE.T3.AND.t.LE.T4) then
     NB(i)=0.5D0*(cos(pi*(t-T3)/(T4-T3))+1.0D0)
   else
     NB(i)=0.0
   end if
 end do

 open(11,file='nb-data',status='replace')
 do i=1,int(NPTS)
   write(11,*) NB(i)
 end do
 close(11)
 
end program
 