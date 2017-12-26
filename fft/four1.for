      Program FFT
      
      integer NN,NN2
      real, allocatable:: t(:),f_t(:),f(:),F_f(:),datain(:)
      character*12 file_in,file_out
      character*64 header

c     +++++++++++++++++++++++++++++++++++++++++++++++++++++++
c     +                                                     +
c     + This routine, FFT.FOR, is based on the routine      +
c     + FOUR1, from the book Numerical Recipes in FORTRAN   +
c     + (Cambridge University Press), Copyright (C) 19   by +
c     + Numerical Recipes Software.  Used by permission.    +
c     + Use of this routine other than as an integral part  +
c     + of FFT.FOR requires an additional license from      +
c     + Numerical Recipes Software.  Further distribution   +
c     + in any form is prohibited.                          +
c     +                                                     +
c     +++++++++++++++++++++++++++++++++++++++++++++++++++++++










C     Prompt user for input and output file names.
      WRITE (*, '(A\)') ' Enter input file name:  '
      READ  (*, '(A)') file_in
      OPEN (8, FILE = file_in)
      
      WRITE (*, '(A\)') ' Enter output file name: '
      READ  (*, '(A)') file_out
      OPEN (9, FILE = file_out, ACCESS = 'SEQUENTIAL',
     +      STATUS = 'NEW')
      
      read(8,'(A)')header
      read(8,*)isign
      read (8,*)dt,NN
      NN2=2*NN


      allocate (t(NN),f_t(NN2),f(NN),F_f(NN2),datain(NN2))

      do 10n=1,NN2-1,2
   10 read(8,*)datain(n),datain(n+1)
    3 format(1x,f12.5,1x,f12.5)

      df=1./dt
      pi=4.0*atan(1.0) 
      
      do 200n=1,NN2-1,2
      m=(n+1)/2
      t(m)=dt*(m-1)
      f(m)=df*(m-1)/NN
      if(m .gt. NN/2)f(m)=-f(NN-m+2)
      if(isign .eq. 1)then
      f_t(n)=datain(n)
      f_t(n+1)=datain(n+1)
      else
      F_f(n)=datain(n)
      F_f(n+1)=datain(n+1)
      endif
  200 continue

      call four1(datain,NN,isign)
      scale=1.0
      if(isign .eq. 1)scale=float(NN)
      do 500n=1,NN2
      datain(n)=datain(n)/scale
      if(isign .eq. 1) F_f(n)=datain(n)
  500 if(isign .eq. -1)f_t(n)=datain(n)
  
      call fft_out(t,f_t,f,F_f,nn,isign,dt,file_in,header)

      go to 1000
c     This is an inverse transform on frequency components.
      isign=-1
      
      do 600n=1,NN2-1,2
      m=(n+1)/2
      t(m)=dt*(m-1)
      f(m)=df*(m-1)/NN
      if(m .gt. NN/2)f(m)=-f(NN-m+2)
      if(isign .eq. 1)then
      f_t(n)=datain(n)
      f_t(n+1)=datain(n+1)
      else
      F_f(n)=datain(n)
      F_f(n+1)=datain(n+1)
      endif
  600 continue

      call four1(datain,NN,isign)
      scale=1.0      
      if(isign .eq. 1)scale=float(NN)      
      do 700n=1,NN2
      datain(n)=datain(n)/scale
      if(isign .eq. 1) F_f(n)=datain(n)
  700 if(isign .eq. -1)f_t(n)=datain(n)

      call fft_out(t,f_t,f,F_f,nn,isign,dt,file_in,header)
 1000 continue

      deallocate(t,f_t,f,F_f,datain)

      end
                   
c =====================================================================

      subroutine fft_out(t,f_t,f,F_f,NN,isign,dt,file_in,header)
      character*12 file_in
      character*64 header
      integer NN
      real t(NN),f_t(2*NN),f(NN),F_f(2*NN)
      
      write(9,100)file_in
  100 format(1x,'Input file name:    ',A12)  
      write(9,110)header
  110 format(1x,'Input file header: ',A64)
      write(9,200)nn,isign,dt,0.5*(1./dt)
  200 format(1x,'number of points: ',i12,/
     .       1x,'isign:            ',i12,/
     .       1x,'time step:        ',e12.5,/
     .       1x,'Nyquist frequency:',e12.5,/)
      
      write(9,300)
	write(9,350)
  300 format(20x,'fn(time)',28x,'Fn(freq)')
  350 format(4x,'time',8x,'real',8x,'imag',8x,'freq',8x,'real',8x,
     .'imag')
      do 400n=1,2*nn-1,2
  400 write(9,500)t((n+1)/2),f_t(n),f_t(n+1),f((n+1)/2),F_f(n),F_f(n+1)
  500 format(1x,6(e11.4,1x))
  
      return
      end
                   
      SUBROUTINE four1(datain,nn,isign)
      INTEGER isign,nn
      REAL datain(2*nn)
      INTEGER i,istep,j,m,mmax,n
      REAL tempi,tempr
      DOUBLE PRECISION theta,wi,wpi,wpr,wr,wtemp
      n=2*nn
      j=1
      do 11 i=1,n,2
        if(j.gt.i)then
          tempr=datain(j)
          tempi=datain(j+1)
          datain(j)=datain(i)
          datain(j+1)=datain(i+1)
          datain(i)=tempr
          datain(i+1)=tempi
        endif
        m=n/2
1       if ((m.ge.2).and.(j.gt.m)) then
          j=j-m
          m=m/2
        goto 1
        endif
        j=j+m
11    continue
      mmax=2
2     if (n.gt.mmax) then
        istep=2*mmax
        theta=6.28318530717959d0/(isign*mmax)
        wpr=-2.d0*sin(0.5d0*theta)**2
        wpi=sin(theta)
        wr=1.d0
        wi=0.d0
        do 13 m=1,mmax,2
          do 12 i=m,n,istep
            j=i+mmax
            tempr=sngl(wr)*datain(j)-sngl(wi)*datain(j+1)
            tempi=sngl(wr)*datain(j+1)+sngl(wi)*datain(j)
            datain(j)=datain(i)-tempr
            datain(j+1)=datain(i+1)-tempi
            datain(i)=datain(i)+tempr
            datain(i+1)=datain(i+1)+tempi
12        continue
          wtemp=wr
          wr=wr*wpr-wi*wpi+wr
          wi=wi*wpr+wtemp*wpi+wi
13      continue
        mmax=istep
      goto 2
      endif
      return
      END
C  (C) Copr. 1986-92 NNPerical Recipes Software.
