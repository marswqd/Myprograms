program testing
 implicit none

 integer*4::i
 integer*4,parameter::k=6,nn=2**k    ! 原始序列必须是2的幂.
 integer*4,parameter::n=11,nz=nn-n
 real*4::ddata(nn),cr(nn),ci(nn),xr(nn),xi(nn),zr(nn),zi(nn)
 real*4::PR(nn),PI(nn),FR(nn),FI(nn)

 !data ddata/5.0e0,32.0e0,38.0e0,-33.0e0,-19.0e0,-10.0e0,      &
           ! 1.0e0,-8.0e0,-20.0e0,10.0e0,-1.0e0,4.0e0,11.0e0,  &
           ! -1.0e0,-7.0e0,-2.0e0/
 data ddata/5.0e0,32.0e0,38.0e0,-33.0e0,-19.0e0,-10.0e0,      &
            1.0e0,-8.0e0,-20.0e0,10.0e0,-1.0e0,nz*0.0e0/
 open(10,file='KKFFT.out')
!傅里叶变换和反变换
 PR=0.0e0
 PI=0.0e0                          ! 虚部赋零
 FR=0.0e0
 FI=0.0e0
 PR=ddata
 call KKFFT(PR,PI,NN,k,FR,FI,0,0)  ! 傅立叶变换

 xr=FR
 xi=FI

 PR=xr
 PI=xi
 call KKFFT(PR,PI,NN,k,FR,FI,1,0)  ! 反变换后应该等于原始数据
 write(10,601)
 601 format('example wave'/37x,'-- fourier and inverse transforms --'/ &
            4x,'m',4x,'data',6x,'k',4x,'fourier transform : real-imag amp-phase', &
            4x,'inverse transform'/)
 do i=1,nn
  write(10,602) i-1,ddata(i),i-1,xr(i),xi(i),sqrt(xr(i)**2+xi(i)**2),     &
                atan(xi(i)/xr(i)),FR(i),FI(i)
 enddo
 602 format(i5,f9.3,i6,3(f10.3,f10.3,2x))
!计算解析函数
 PR=xr
 PI=xi
 PR(1)=PR(1)/2            ! 由于FFT得到的是单边谱，是双边谱的2倍
 PI(1)=PI(1)/2            ! 计算解析函数时，频率=0的点应除以2
 PR(nn/2+1)=PR(nn/2+1)/2       ! 由于截止频率的谱是实数, 正负频率的贡献是相同的.
 PI(nn/2+1)=PI(nn/2+1)/2       ! 所以, 正频率的谱应该是计算值的一半.
 PR(nn/2+2:nn)=0.0e0           ! 负频率赋为0
 PI(nn/2+2:nn)=0.0e0
 call KKFFT(PR,PI,NN,k,FR,FI,1,1)  ! 反变换后序列的2倍等于原始序列的解析序列, 其实部为原始序列, 虚部为其hilbert变换.
 write(10,603)
 603 format(/5x,'analytic function'/7x,'m',5x,'real',7x,'imag',7x,'amp',5x,'phase'/)
 do i=1,nn
  write(10,'(i8,4f10.3)') i-1,2*FR(i),2*FI(i),PR(i),PI(i)     ! 输出结果后, 可验证其正确性.
 enddo
!谱白化
 PR=xr
 PI=xi
 do i=1,nn/2+1
  if(xr(i).eq.0.0e0.and.xi(i).eq.0.0e0) then
   PR(i)=1.0e0
   PI(i)=0.0e0
  else
   PR(i)=PR(i)/sqrt(xr(i)**2+xi(i)**2)
   PI(i)=PI(i)/sqrt(xr(i)**2+xi(i)**2)
  endif
 enddo
 PR(1)=PR(1)/2            ! 由于FFT得到的是单边谱，是双边谱的2倍
 PI(1)=PI(1)/2            ! 计算解析函数时，频率=0的点应除以2
 PR(nn/2+1)=PR(nn/2+1)/2       ! 由于截止频率的谱是实数, 正负频率的贡献是相同的.
 PI(nn/2+1)=PI(nn/2+1)/2       ! 所以, 正频率的谱应该是计算值的一半.
 PR(nn/2+2:nn)=0.0e0           ! 负频率赋为0
 PI(nn/2+2:nn)=0.0e0
 write(10,604)
 604 format(/5x,'white spectrum'/7x,'m',5x,'real',7x,'imag',7x,'amp',5x,'phase'/)
 do i=1,nn
  write(10,'(i8,4f10.3)') i-1,PR(i),PI(i),sqrt(PR(i)**2+PI(i)**2),atan(PI(i)/PR(i))
 enddo
!谱白化-ifft
 call KKFFT(PR,PI,NN,k,FR,FI,1,1)

 cr=FR
 ci=FI

 write(10,605)
 605 format(/5x,'white spectrum ifft'/7x,'m',5x,'real',7x,'imag',7x,'amp',5x,'phase'/)
 do i=1,nn
  write(10,'(i8,4f10.3)') i-1,2*FR(i),2*FI(i),PR(i),PI(i)
 enddo
!谱白化-ifft-fft
 call KKFFT(FR,FI,NN,k,PR,PI,0,0)
 write(10,606)
 606 format(/5x,'white spectrum ifft-fft'/7x,'m',5x,'real',7x,'imag',5x,'amp'/)
 do i=1,nn
  write(10,'(i8,3f10.3)') i-1,PR(i),PI(i),sqrt(PR(i)**2+PI(i)**2)
 enddo
!sw-ifft-real-fft
 PR=2*cr
 PI=0.0
 call KKFFT(PR,PI,NN,k,FR,FI,0,1)
 write(10,607)
 607 format(/5x,'sw-ifft-real-fft'/7x,'m',5x,'data',5x,'fourier transform', &
            5x,'amp',5x,'phase'/)
 do i=1,nn
  write(10,'(i8,5f10.3)') i-1,2*cr(i),FR(i),FI(i),PR(i),PI(i)
 enddo
!sw-ifft-data-fft
 PR=0.0
 PI=0.0
 PR(1:n)=2*cr(1:n)
 zr=PR
 call KKFFT(PR,PI,NN,k,FR,FI,0,1)
 write(10,608)
 608 format(/5x,'sw-ifft-data-fft'/7x,'m',5x,'data',5x,'fourier transform', &
            5x,'amp',5x,'phase'/)
 do i=1,nn
  write(10,'(i8,5f10.3)') i-1,zr(i),FR(i),FI(i),PR(i),PI(i)
 enddo

 !deallocate
end program


SUBROUTINE KKFFT(PR,PI,N,K,FR,FI,L,IL)
!L=0 for fft;L=1 for ifft
!IL=0 不计算模与幅角;IL=1 计算模与幅角
 IMPLICIT NONE
 INTEGER*4::I,N,K,IT,L,IL,M,IS,J,NV,L0
 REAL*4::PR(N),PI(N),FR(N),FI(N),P,Q,S,VR,VI,PODDR,PODDI
 DO IT=0,N-1
  M=IT
	IS=0
	DO I=0,K-1
	 J=M/2
	 IS=2*IS+(M-2*J)
	 M=J
  ENDDO
	FR(IT+1)=PR(IS+1)
	FI(IT+1)=PI(IS+1)
 ENDDO
 PR(1)=1.0E0
 PI(1)=0.0E0
 PR(2)=COS(6.283185306E0/N)
 PI(2)=-SIN(6.283185306E0/N)
 IF(L.NE.0) PI(2)=-PI(2)
 DO I=3,N
	P=PR(I-1)*PR(2)
	Q=PI(I-1)*PI(2)
	S=(PR(I-1)+PI(I-1))*(PR(2)+PI(2))
	PR(I)=P-Q
	PI(I)=S-P-Q
 ENDDO
 DO IT=0,N-2,2
	VR=FR(IT+1)
	VI=FI(IT+1)
	FR(IT+1)=VR+FR(IT+2)
	FI(IT+1)=VI+FI(IT+2)
	FR(IT+2)=VR-FR(IT+2)
	FI(IT+2)=VI-FI(IT+2)
 ENDDO
 M=N/2
 NV=2
 DO L0=K-2,0,-1
	M=M/2
	NV=2*NV
	DO IT=0,(M-1)*NV,NV
	 DO J=0,(NV/2)-1
	  P=PR(M*J+1)*FR(IT+J+1+NV/2)
	  Q=PI(M*J+1)*FI(IT+J+1+NV/2)
	  S=PR(M*J+1)+PI(M*J+1)
	  S=S*(FR(IT+J+1+NV/2)+FI(IT+J+1+NV/2))
	  PODDR=P-Q
	  PODDI=S-P-Q
	  FR(IT+J+1+NV/2)=FR(IT+J+1)-PODDR
    FI(IT+J+1+NV/2)=FI(IT+J+1)-PODDI
	  FR(IT+J+1)=FR(IT+J+1)+PODDR
	  FI(IT+J+1)=FI(IT+J+1)+PODDI
   ENDDO
  ENDDO
 ENDDO
 IF(L.NE.0) THEN
  DO I=1,N
	 FR(I)=FR(I)/N
	 FI(I)=FI(I)/N
  ENDDO
 ENDIF
 IF(IL.NE.0) THEN
  DO I=1,N
	 PR(I)=SQRT(FR(I)*FR(I)+FI(I)*FI(I))
   PI(I)=ATAN(FI(I)/FR(I))*360.0E0/6.283185306E0
   !将相位还原为[0,2pai],即[0,360]
   !IF(FR(I).LT.0.0E0) PI(I)=180.0E0+PI(I)
   !IF(FI(I).LE.0E0.AND.FR(I).GE.0.0E0) PI(I)=360.0E0+PI(I)
  ENDDO
 ENDIF
 RETURN
END SUBROUTINE



