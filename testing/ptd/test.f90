program test
 implicit none
 integer*4::i,j
 integer*4,parameter::n=101
 real*4::a(n),b(n),c1(-n+1:n-1),c2(n+n-1),c3(n+n-1)

 open(11,file='DATA.TXT')
 do i=1,n
  read(11,*) a(i),b(i)
 enddo
 close(11)

 b(1:9)=a(n-8:n)
 b(10:n)=a(1:n-9)

 call tcor(a,n,b,n,c1,-n+1,n-1)
 call fcor(b,n,a,n,c2)
 call scor(b,n,a,n,c3)

 open(11,file='COR3.TXT')
 do i=1,n+n-1
  write(11,*) c1(i-n),c2(i),c3(i),c2(i)-c1(i),c3(i)-c1(i)
 enddo
 close(11)


end program



SUBROUTINE TCOR(X,M,H,N,Y,LN,RN)
 IMPLICIT NONE
! This program is purposed to calculate the auto-correlation or
! corss-correlation function in time-domain. Creatied: 2010/12/17 15:25:44
 INTEGER*4::I,J,M,N,LN,RN
 REAL*4::X(M),H(N),Y(LN:RN)
! X(M),H(N)为输入，Y(-M+1:N-1)为输出；Y为X与H的相关序列，共M+N-1项
! Y(0)  为X(M),H(N)首项对齐时的相关值；
! Y(1)  为H(N)相对X(M)左移一项时的相关值，正向分支共N-1项；
! Y(-1) 为H(N)相对X(M)右移一项时的相关值，负向分支共M-1项。
! LN表示所求的相关序列的左边界，RN表示所求的相关序列的右边界。
 IF(LN.LT.-M+1) PRINT*, '左边界超出'
 IF(RN.GT.N-1) PRINT*, '右边界超出'
 DO I=LN,RN
  Y(I)=0.0e0
  DO J=1,M
   IF(J+I.GE.1.AND.J+I.LE.N) Y(I)=Y(I)+X(J)*H(J+I)
  ENDDO
 ENDDO
 RETURN
END SUBROUTINE


SUBROUTINE FCOR(X,M,H,N,Y)
 IMPLICIT NONE
! THIS PROGRAM IS PURPOSED TO CALCULATE THE AUTO-CORRELATION OR
! CORSS-CORRELATION FUNCTION IN FREQUENCE-DOMAIN. CREATIED: 2011-01-04 20:20:27
 INTEGER*4::I,M,N,LOGN,NFFT
 REAL*4::X(M),H(N),Y(M+N-1)
 REAL*4,ALLOCATABLE::PR(:),PI(:),FR(:),FI(:),HFR(:),HFI(:)
! X(M),H(N)为输入，Y(M+N-1)为输出；Y为X与H的相关序列，共M+N-1项
 !LOGN=FLOOR(LOG10(M+N-1.0E0)/LOG10(2.0E0))+1
 !NFFT=2**LOGN
 CALL NPOW2(M+N-1,NFFT,LOGN)
 !PRINT*, LOGN,NFFT
 ALLOCATE(PR(NFFT),PI(NFFT),FR(NFFT),FI(NFFT),HFR(NFFT),HFI(NFFT))
 PR=0.0E0
 PI=0.0E0
 FR=0.0E0
 FI=0.0E0
 HFR=0.0E0
 HFI=0.0E0
 CALL TAPER(X,M,0.005)
 PR(1:M)=X(1:M)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,0,0)
 PR=0.0E0
 PI=0.0E0
 PR(1:N)=H(1:N)
 CALL KKFFT(PR,PI,NFFT,LOGN,HFR,HFI,0,0)
 DO I=1,NFFT
  PR(I)=FR(I)*HFR(I)+FI(I)*HFI(I)
  PI(I)=FR(I)*HFI(I)-FI(I)*HFR(I)
 ENDDO
 OPEN(11,FILE='FCORF.TXT')
 DO I=1,NFFT
  WRITE(11,*) PR(I),PI(I),SQRT(PR(I)*PR(I)+PI(I)*PI(I)),ATAN2(PI(I),PR(I))
 ENDDO
 CLOSE(11)
 CALL TAPER(PR,NFFT,0.005)
 CALL TAPER(PI,NFFT,0.005)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,1,0)
! OUT(1:NLEN1-1)=YTMP(NFFT-NLEN1+2:NFFT)
! OUT(NLEN1:NLEN1+NLEN2-1)=YTMP(1:NLEN2)
 OPEN(11,FILE='FCOR.TXT')
 DO I=1,NFFT
  WRITE(11,*) FR(I),FI(I),SQRT(FR(I)*FR(I)+FI(I)*FI(I)),ATAN2(FI(I),FR(I))
 ENDDO
 CLOSE(11)
 DO I=1,M-1
  Y(I)=FR(NFFT-M+I+1)
 ENDDO
 DO I=1,N
  Y(M+I-1)=FR(I)
 ENDDO
 DEALLOCATE(PR,PI,FR,FI,HFR,HFI)
 RETURN
END SUBROUTINE


SUBROUTINE SCOR(X,M,H,N,Y)
 IMPLICIT NONE
! THIS PROGRAM IS PURPOSED TO CALCULATE THE AUTO-CORRELATION OR
! CORSS-CORRELATION FUNCTION IN FREQUENCE-DOMAIN(FORM SAC2000). CREATIED: 2011-01-04 20:20:31
 INTEGER*4::I,J,M,N,LOGN,NFFT
 REAL*4::X(M),H(N),Y(M+N-1),XR,XI,YR,YI
 REAL*4,ALLOCATABLE::PR(:),PI(:),FR(:),FI(:)
! X(M),H(N)为输入，Y(M+N-1)为输出；Y为X与H的相关序列，共M+N-1项
 !LOGN=FLOOR(LOG10(M+N-1.0E0)/LOG10(2.0E0))+1
 !NFFT=2**LOGN
 CALL NPOW2(M+N-1,NFFT,LOGN)
 !PRINT*, LOGN,NFFT
 ALLOCATE(PR(NFFT),PI(NFFT),FR(NFFT),FI(NFFT))
 PR=0.0E0
 PI=0.0E0
 FR=0.0E0
 FI=0.0E0
 CALL TAPER(X,M,0.005)
 CALL TAPER(H,N,0.005)
 PR(1:M)=X(1:M)
 PI(1:N)=H(1:N)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,0,0)
 PR=0.0E0
 PI=0.0E0
 PR(1)=FR(1)*FI(1)
 DO I=2,NFFT/2+1
  J=NFFT-I+2
  XR=(FR(I)+FR(J))/2
	XI=(FI(I)-FI(J))/2
	YR=(FI(I)+FI(J))/2
	YI=(FR(J)-FR(I))/2
	PR(I)=XR*YR+XI*YI
  PI(I)=XR*YI-XI*YR
	PR(J)=PR(I)
	PI(J)=-PI(I)
 ENDDO
 OPEN(11,FILE='SCORF.TXT')
 DO I=1,NFFT
  WRITE(11,*) PR(I),PI(I),SQRT(PR(I)*PR(I)+PI(I)*PI(I)),ATAN2(PI(I),PR(I))
 ENDDO
 CALL TAPER(PR,NFFT,0.005)
 CALL TAPER(PI,NFFT,0.005)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,1,0)
! Y(1:NLEN1-1)=FR(NFFT-NLEN1+2:NFFT)
! Y(NLEN1:NLEN1+NLEN2-1)=FR(1:NLEN2)
 OPEN(11,FILE='SCOR.TXT')
 DO I=1,NFFT
  WRITE(11,*) FR(I),FI(I),SQRT(FR(I)*FR(I)+FI(I)*FI(I)),ATAN2(FI(I),FR(I))
 ENDDO
 CLOSE(11)
 DO I=1,M-1
  Y(I)=FR(NFFT-M+I+1)
 ENDDO
 DO I=1,N
  Y(M+I-1)=FR(I)
 ENDDO
 DEALLOCATE(PR,PI,FR,FI)
 RETURN
END SUBROUTINE


subroutine taper(data,n,width)
!仿照sac2000中的taper命令
!DATA(J)=DATA(J)*(F0-F1*COS(OMEGA*(J-1))
!======== ========= ===== ======
!TYPE     OMEGA     F0    F1
!======== ========= ===== ======
!HANNING   PI/N     0.50  0.50
!HAMMING   PI/N     0.54  0.46
!COSINE    PI/(2*N) 1.00  1.00
!======== ========= ===== ======
!这里选用HANNING
 implicit none
 integer*4::i,j,k,n,tl,tr
 real*4::data(n),width,f0,f1,omega
 real*4,parameter::pi=3.14159265358e0
 do i=1,n
  if(data(i).ne.0.0e0) exit
 enddo
 if(i.eq.n) then
  print*, 'This is a zero data, please check'
  return
 endif
 tl=i
 do i=n,1,-1
  if(data(i).ne.0.0e0) exit
 enddo
 tr=i
 f0=0.5e0
 f1=0.5e0
 j=anint((tr-tl+1)*width)
 if(j.eq.1.or.j.eq.0) return
 omega=pi/j
 do i=tl,tl+j-1
  data(i)=data(i)*(f0-f1*cos(omega*(i-tl)))
 enddo
 do i=tr,tr-j+1,-1
  data(i)=data(i)*(f0-f1*cos(omega*(i-tr)))
 enddo
 return
end subroutine


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
   PI(I)=ATAN2(FI(I),FR(I))*360.0E0/6.283185306E0
   !将相位还原为[0,2pai],即[0,360]
   IF(PI(I).LT.0.0E0) PI(I)=360.0E0+PI(I)
  ENDDO
 ENDIF
 RETURN
END SUBROUTINE


subroutine npow2(n,nfft,logn)
 implicit none
 integer*4::n,nfft,logn
 nfft=1
 logn=0
 do while(nfft.lt.n)
  nfft=2*nfft
  logn=logn+1
 enddo
 return
end subroutine


