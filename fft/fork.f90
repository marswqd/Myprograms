subroutine fork(lx,cx,signi)
! fast fourier,2/15/69; signi=-1:fft , signi=1:ifft .
!                  lx
! cx(k)=sqrt(1/lx) sum (cx(j)*exp(2*pi*signi*i*(j-1)*(k-1)/lx))
!                  j=1           for k= 1,2,...,(lx=2**integer)
   implicit none
   integer*4::signi,i,j,lx,m,l,istep
   real*4::sc
   complex(4)::cx(lx),carg,cexp,cw,ctemp
   j=1
   sc=sqrt(1.0e0/lx)
   do 30 i=1,lx
   if(i.gt.j) goto 10
   ctemp=cx(j)*sc
   cx(j)=cx(i)*sc
   cx(i)=ctemp
10 m=lx/2
20 if(j.le.m) goto 30
   j=j-m
   m=m/2
   if(m.ge.1) goto 20
30 j=j+m
   l=1
40 istep=2*l
   do 50 m=1,l
   carg=(0.0e0,1.0e0)*(3.14159265358e0*signi*(m-1))/l
   cw=cexp(carg)
   do 50 i=m,lx,istep
   ctemp=cw*cx(i+l)
   cx(i+l)=cx(i)-ctemp
50 cx(i)=cx(i)+ctemp
   l=istep
   if(l.lt.lx) goto 40
   return
end subroutine


program testing
	  complex(4)::c(16),x(16),test
	  real*4::ddata(16),amplitude(16)
	  data ddata/5.0e0,32.0e0,38.0e0,-33.0e0,-19.0e0,-10.0e0,      &
               1.0e0,-8.0e0,-20.0e0,10.0e0,-1.0e0,4.0e0,11.0e0,  &
               -1.0e0,-7.0e0,-2.0e0/,nn/16/     !原始序列必须是2的幂.
	  open(6,file='fork.out')
	  do 110 m=1,nn
	   c(m)=cmplx(ddata(m),0.0)    ! 虚部赋零
110 continue
	  call fork(nn,c,-1)           ! 傅立叶变换
    do 120 k=1,nn
	  !c(k)=c(k)/float(nn)         ! 由于子程序fast在计算正变换时乘了nn(数据的长度), 所以真正的谱应该除以nn.
	  x(k)=c(k)
120 continue
	  call fork(nn,x,+1)           ! 反变换后应该等于原始数据
    write(6,601)
	  do 130 mk=1,nn
    	mk1=mk-1
	    write(6,602) mk1,ddata(mk),mk1,c(mk),x(mk)
130 continue
	  c(nn/2+1)=.5*c(nn/2+1)       ! 由于截至频率的谱是实数, 正负频率的贡献是相同的.所以, 正频率的谱应该是计算值的一半.
	  do 140 i=nn/2+2, nn          ! 负频率的谱设置为零.
140 c(i)=(0.,0.)
	  call fork(nn,c,+1)           ! 反变换后序列的2倍等于原始序列的解析序列, 其实部为原始序列, 虚部为其hilbert变换.
    do 150 mk=1,nn
	   mk1=mk-1
	   amplitude(mk)=cabs(c(mk))
150	write(6,'(i8,3f10.3)') mk1,2.*c(mk),amplitude(mk)     ! 输出结果后, 可验证其正确性.
	  stop
601 format(1h,30x,12hexample wave/1h,35x,36h-- fourier and inverse transforms --/ &
           1h0/1h,36x,1hm,3x,4hdata,7x,1hk,4x,17hfourier transform, &
           2x,17hinverse transform/)
602 format(1h,35x,i2,f7.0,i8,5x,2f7.3,5x,2f7.3)
end program
