subroutine fast(n,x,ind)
!ind=-1:fft ; ind=1:ifft . n������2����.
    implicit none
    integer*4::i,j,k,m,n,ind,kmax,istep
    complex(4)::x(n),temp,theta
	  j=1
	  do 140 i=1,n
	  if(i.ge.j) goto 110
	  temp=x(j)
	  x(j)=x(i)
	  x(i)=temp
110 m=n/2
120 if(j.le.m) goto 130
	  j=j-m
	  m=m/2
	  if(m.ge.2) goto 120
130 j=j+m
140 continue
	  kmax=1
150 if(kmax.ge.n) goto 180
	  istep=kmax*2
	  do 170 k=1,kmax
	   theta=cmplx(0.0e0,3.14159265358e0)*float(ind*(k-1))/float(kmax)
	   do 160 i=k,n,istep
	    j=i+kmax
	    temp=x(j)*cexp(theta)
	    x(j)=x(i)-temp
	    x(i)=x(i)+temp
160  continue
170 continue
	  kmax=istep
	  goto 150
180 if(ind.eq.-1) goto 200
    do 190 i=1,n
     x(i)=x(i)/float(n)
190 continue
200 return
end subroutine


program testing
	  complex(4)::c(16),x(16),test
	  real*4::ddata(16),amplitude(16)
	  !data ddata/5.0e0,32.0e0,38.0e0,-33.0e0,-19.0e0,-10.0e0,      &
              ! 1.0e0,-8.0e0,-20.0e0,10.0e0,-1.0e0,4.0e0,11.0e0,  &
              ! -1.0e0,-7.0e0,-2.0e0/,nn/16/     !ԭʼ���б�����2����.
    data ddata/5.0e0,32.0e0,38.0e0,-33.0e0,-19.0e0,-10.0e0,      &
            1.0e0,-8.0e0,-20.0e0,10.0e0,-1.0e0,0.0e0,0.0e0,  &
            0.0e0,0.0e0,0.0e0/,nn/16/


	  open(6,file='fast.out')
	  do 110 m=1,nn
	   c(m)=cmplx(ddata(m),0.0)    ! �鲿����
110 continue
	  call fast(nn,c,-1)           ! ����Ҷ�任
    do 120 k=1,nn
	  !c(k)=c(k)/float(nn)         ! �����ӳ���fast�ڼ������任ʱ����nn(���ݵĳ���), ������������Ӧ�ó���nn.
	  x(k)=c(k)
120 continue
	  call fast(nn,x,+1)           ! ���任��Ӧ�õ���ԭʼ����
    write(6,601)
	  do 130 mk=1,nn
    	mk1=mk-1
	    write(6,602) mk1,ddata(mk),mk1,c(mk),x(mk)
130 continue
    c(1)=.5*c(1)
	  c(nn/2+1)=.5*c(nn/2+1)       ! ���ڽ���Ƶ�ʵ�����ʵ��, ����Ƶ�ʵĹ�������ͬ��.����, ��Ƶ�ʵ���Ӧ���Ǽ���ֵ��һ��.
	  do 140 i=nn/2+2, nn          ! ��Ƶ�ʵ�������Ϊ��.
140 c(i)=(0.,0.)
    !do 140 i=nn/2+2,nn
!140 c(i)=(0.,0.)                 ! ��Ƶ�ʵ�������Ϊ��.
   ! do 160 i=2,nn/2
!160 c(i)=c(i)*2                  ! ��Ƶ�ʵ��׳���2.

    !c=c/2
    call fast(nn,c,+1)           ! ���任�����е�2������ԭʼ���еĽ�������, ��ʵ��Ϊԭʼ����, �鲿Ϊ��hilbert�任.
    do 150 mk=1,nn
	   mk1=mk-1
	   amplitude(mk)=cabs(c(mk))
150	write(6,'(i8,3f10.3)') mk1,2*c(mk),amplitude(mk)     ! ��������, ����֤����ȷ��.
	  stop
601 format(32x,'example wave'/37x,'-- fourier and inverse transforms --'/ &
            36x,'m',4x,'data',6x,'k',4x,'fourier transform', &
            3x,'inverse transform'/)
602 format(1h,35x,i2,f7.0,i8,5x,2f7.3,5x,2f7.3)
end program
