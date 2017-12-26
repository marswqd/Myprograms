program randnum
 implicit none
 integer*4::i
 real*4::a(10000)
 open(10,file='randnum5.txt')

 call my_random(-300.0,300.0,a,10000)
 do i=1,10000
  write(10,*) a(i)
 enddo
end program


subroutine my_random(lbound,ubound,a,size)
 implicit none
 integer*4::i,size!size代表数组元素的个数
 real*4::lbound,ubound,len,t,a(size)
 len=ubound-lbound  !计算范围大小
 !call random_seed()!系统根据日期和时间随机地提供种子
 call init_random_seed() !来自gfortran random_seed() Example
 do i=1,size
  call random_number(t)  !t是0-1之间的随机数
  a(i)=lbound+len*t  !把t转换成lbound-ubound间的随机数
 enddo
 return
end subroutine


subroutine init_random_seed()
 use iso_fortran_env,only: int64
 implicit none
 integer,allocatable::seed(:)
 integer::i,n,un,istat,dt(8),pid
 integer(int64)::t
 call random_seed(size=n)
 allocate(seed(n))
 ! First try if the OS provides a random number generator
 open(newunit=un,file="/dev/urandom",access="stream", &
      form="unformatted",action="read",status="old",iostat=istat)
 if(istat==0) then
  read(un) seed
  close(un)
 else
 ! Fallback to XOR:ing the current time and pid. The PID is
 ! useful in case one launches multiple instances of the same
 ! program in parallel.
  call system_clock(t)
  if(t==0) then
   call date_and_time(values=dt)
   t=(dt(1)-1970)*365_int64*24*60*60*1000 &
     +dt(2)*31_int64*24*60*60*1000 &
     +dt(3)*24_int64*60*60*1000 &
     +dt(5)*60*60*1000 &
     +dt(6)*60*1000+dt(7)*1000 &
     +dt(8)
  endif
  pid=getpid()
  t=ieor(t,int(pid,kind(t)))
  do i=1,n
   seed(i)=lcg(t)
  enddo
 endif
 call random_seed(put=seed)
 contains
 ! This simple PRNG might not be good enough for real work, but is
 ! sufficient for seeding a better PRNG.
 function lcg(s)
  integer::lcg
  integer(int64)::s
  if(s==0) then
   s=104729
  else
   s=mod(s,4294967296_int64)
  endif
  s=mod(s*279470273_int64,4294967291_int64)
  lcg=int(mod(s,int(huge(0),int64)),kind(0))
 end function lcg
end subroutine init_random_seed


