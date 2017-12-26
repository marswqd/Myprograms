Module ran_mod
 Implicit None
! ran return a uniform random number between 0-1
! norma return a normal distribution
 contains
 function ran()   !returns random number between 0 - 1
  implicit none
  integer , save :: flag = 0
  double precision :: ran
  if(flag==0) then
   call random_seed()
   flag = 1
  endif
  call random_number(ran)     ! built in fortran 90 random number function
 end function ran

 function normal(mean,sigma)
  implicit none
  integer :: flag
  double precision, parameter :: pi = 3.141592653589793239
  double precision :: u1, u2, y1, y2, normal, mean, sigma
  save flag
  data flag /0/
  u1 = ran(); u2 = ran()
  if (flag.eq.0) then
   y1 = sqrt(-2.0d0*log(u1))*cos(2.0d0*pi*u2)
   normal = mean + sigma*y1
   flag = 1
  else
   y2 = sqrt(-2.0d0*log(u1))*sin(2.0d0*pi*u2)
   normal = mean + sigma*y2
   flag = 0
  endif
 end function normal
 !The above codes are made in Fortran 90 language, if you have any question,
 !you may write to sealin2008@hotmail.com
End Module ran_mod


Program www_fcode_cn
 use ran_mod
 Implicit None
 Integer , parameter :: N = 10000
 Real( Kind = 8 ) :: a( N )
 integer :: i
 Open( 12 , File = 'out.txt' )
 Do i = 1 , N
  a( i ) = normal( 5.0D0 , 2.0D0 )
  write( 12 , * ) a(i)
 End Do
 Close( 12 )
End Program www_fcode_cn
