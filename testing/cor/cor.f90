program main
 implicit none
 integer*4::i,NPTSA,NPTSB,NPTSC,nerr
 real*4::width
 character*60::sacA,sacB

 real*4,allocatable::dataA(:),dataB(:),dataC(:),dataTC(:),dataFC(:),dataSC(:)
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)

 width=0.005e0
 print*, 'm  width=',width
 sacA='E0001S1.0.SAC'
 sacB='E0001S2.0.SAC'
 call brsach(100,sacA,nerr)
 NPTSA=ihdr(10)
 allocate(dataA(NPTSA))
 call brsac(100,NPTSA,sacA,dataA,nerr)
 call brsach(100,sacB,nerr)
 NPTSB=ihdr(10)
 allocate(dataB(NPTSB))
 call brsac(100,NPTSB,sacB,dataB,nerr)

 NPTSC=NPTSA+NPTSB-1
 allocate(dataC(-NPTSA+1:NPTSB-1),dataTC(NPTSC),dataFC(NPTSC),dataSC(NPTSC))
 call TCOR(dataA,NPTSA,dataB,NPTSB,dataC,-NPTSA+1,NPTSB-1)
 do i=1,NPTSC
  dataTC(i)=dataC(-NPTSA+i)
 enddo
 call FCOR(dataA,NPTSA,dataB,NPTSB,dataFC,width)
 call SCOR(dataA,NPTSA,dataB,NPTSB,dataSC,width)
 call nor(dataTC,NPTSC)
 call nor(dataFC,NPTSC)
 call nor(dataSC,NPTSC)

 open(10,file='cor.out')
 do i=1,NPTSC
  write(10,*) dataTC(i),dataFC(i),dataSC(i),dataFC(i)/dataTC(i)-1,dataSC(i)/dataTC(i)-1
 enddo
 close(10)

end program


subroutine brsach(IRU,name,nerr)
!-----
!       IRU I*4 logical unit for IO
!       name    C*  Name of file to be opened
!       rhdr    R*4 Real header
!       ihdr    I*4 Integer Header
!       chdr    C*  Character Header
!       nerr    I*4 -1 file does not exist
!               -2 data points in file exceed dimension
!
!       NOTE IF THE BINARY FILE HAS MORE THAN LN POINTS, THEN ONLY
!       LN POINTS ARE USED
!-----
!  This routine reads waveform data written in SAC binary format.
!
!  Written by Hafidh A. A. Ghalib, 1988.
!-----
 implicit none
 integer*4::i,IRU,nerr
 logical::ext
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
!-----
!  Read real and integer header blocks to find actual number
!  of waveform data points which is stored in ihdr(10).
!-----
 inquire(file=name,exist=ext)
 if(.not.ext) then
  ihdr(10)=0
  nerr=-1
  return
 endif
 nerr=0
 !open(IRU,file=name,form='unformatted',access='direct',recl=440,status='old')
 !read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40)
 open(IRU,file=name,form='unformatted',access='direct',recl=632,status='old')
 read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40),(chdr(i),i=1,24)
 close(IRU)
 return
end subroutine


subroutine brsac(IRU,LN,name,data,nerr)
!-----
!       IRU I*4 logical unit for IO
!       LN  I*4 length of data array
!       name    C*  Name of file to be opened
!       rhdr    R*4 Real header
!       ihdr    I*4 Integer Header
!       chdr    C*  Character Header
!       data    R*4 Array of trace values
!       nerr    I*4 -1 file does not exist
!               -2 data points in file exceed dimension
!
!       NOTE IF THE BINARY FILE HAS MORE THAN LN POINTS, THEN ONLY
!       LN POINTS ARE USED
!-----
!  This routine reads waveform data written in SAC binary format.
!
!  Written by Hafidh A. A. Ghalib, 1988.
!-----
 implicit none
 integer*4::i,IRU,LN,nerr,maxpts,nbytes,ndat,nread,ndat1,irec,nl,nh
 real*4::data(LN)
 logical::ext
 common/sachdr/rhdr,ihdr,chdr
 real*4::rhdr(70)
 integer*4::ihdr(40)
 character*8::chdr(24)
 character*(*)::name
!  Read real and integer header blocks to find actual number
!  of waveform data points which is stored in ihdr(10).
! print*, name
 inquire(file=name,exist=ext)
 if(.not.ext) then
  ihdr(10)=0
  nerr=-1
  return
 endif
 nerr=0
 open(IRU,file=name,form='unformatted',access='direct',recl=440,status='old')
 read(IRU,rec=1) (rhdr(i),i=1,70),(ihdr(i),i=1,40)
 close(IRU)
!  Read header and waveform data blocks using recored length of 158*4=632.
 if(ihdr(10).gt.LN) then
  maxpts=LN
  ihdr(10)=LN
  nerr=-2
 else
  maxpts=ihdr(10)
  nerr=0
 endif
 nbytes=632+4*maxpts
 nread=0
!  because of SUNOS Fortran problems with IO transfers
!  more than 2048 bytes, read these  chunks in
 ndat=maxpts
 if(nbytes.gt.2048) then
  open(IRU,file=name,form='unformatted',access='direct',recl=2048)
  ndat1=(2048-632)/4
  irec=1
  read(IRU,rec=irec,err=1001) (rhdr(i),i=1,70),   &
                              (ihdr(i),i=1,40),   &
                              (chdr(i),i=1,24),   &
                              (data(i),i=1,ndat1)
  nread=nread+ndat1
  1000 continue
  nl=nread+1
  nh=nl+512-1
  if(nh.gt.ndat) then
   nh=ndat
  endif
  if(nl.gt.ndat) goto 1001
  irec=irec+1
  read(IRU,rec=irec,err=1001) (data(i),i=nl,nh)
  nread=nread+(nh-nl+1)
  goto 1000
  1001 continue
  close(IRU)
 else
  open(IRU,file=name,form='unformatted',access='direct',recl=nbytes)
  read(IRU,rec=1) (rhdr(i),i=1,70),   &
                  (ihdr(i),i=1,40),   &
                  (chdr(i),i=1,24),   &
                  (data(i),i=1,ndat)
  close(IRU)
 endif
 if(ihdr(10).gt.LN) then
  maxpts=LN
  ihdr(10)=LN
 else
  maxpts=ihdr(10)
 endif
  ihdr(10)=maxpts
 return
end subroutine


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
 if(i.eq.n) return
 tl=i
 do i=n,1,-1
  if(data(i).ne.0.0e0) exit
 enddo
 tr=i
 f0=0.5e0
 f1=0.5e0
 j=anint((tr-tl+1)*width)
 if(j.eq.1.or.j.eq.0) then
  print*, 'a'
  return
 endif
 omega=pi/j
 do i=tl,tl+j-1
  data(i)=data(i)*(f0-f1*cos(omega*(i-tl)))
 enddo
 do i=tr,tr-j+1,-1
  data(i)=data(i)*(f0-f1*cos(omega*(i-tr)))
 enddo
 return
end subroutine


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


subroutine nor(data,n)
 implicit none
 integer*4::n,i
 real*4::data(n),max
 max=0.0e0
 do i=1,n
  if(max.lt.abs(data(i))) max=abs(data(i))
 enddo
 data=data/max
end subroutine


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


SUBROUTINE FCOR(X,M,H,N,Y,width)
 IMPLICIT NONE
! THIS PROGRAM IS PURPOSED TO CALCULATE THE AUTO-CORRELATION OR
! CORSS-CORRELATION FUNCTION IN FREQUENCE-DOMAIN. CREATIED: 2011-01-04 20:20:27
 INTEGER*4::I,M,N,LOGN,NFFT
 REAL*4::X(M),H(N),Y(M+N-1),width
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
 PR(1:M)=X(1:M)
 !CALL TAPER(PR,NFFT,width)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,0,0)
 PR=0.0E0
 PI=0.0E0
 PR(1:N)=H(1:N)
 !CALL TAPER(PR,NFFT,width)
 CALL KKFFT(PR,PI,NFFT,LOGN,HFR,HFI,0,0)
 DO I=1,NFFT
  PR(I)=FR(I)*HFR(I)+FI(I)*HFI(I)
  PI(I)=FR(I)*HFI(I)-FI(I)*HFR(I)
 ENDDO
 CALL TAPER(PR,NFFT,width)
 CALL TAPER(PI,NFFT,width)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,1,0)
! OUT(1:NLEN1-1)=YTMP(NFFT-NLEN1+2:NFFT)
! OUT(NLEN1:NLEN1+NLEN2-1)=YTMP(1:NLEN2)
 DO I=1,M-1
  Y(I)=FR(NFFT-M+I+1)
 ENDDO
 DO I=1,N
  Y(M+I-1)=FR(I)
 ENDDO
 DEALLOCATE(PR,PI,FR,FI,HFR,HFI)
 RETURN
END SUBROUTINE


SUBROUTINE SCOR(X,M,H,N,Y,width)
 IMPLICIT NONE
! THIS PROGRAM IS PURPOSED TO CALCULATE THE AUTO-CORRELATION OR
! CORSS-CORRELATION FUNCTION IN FREQUENCE-DOMAIN(FORM SAC2000). CREATIED: 2011-01-04 20:20:31
 INTEGER*4::I,J,M,N,LOGN,NFFT
 REAL*4::X(M),H(N),Y(M+N-1),XR,XI,YR,YI,width
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
 PR(1:M)=X(1:M)
 PI(1:N)=H(1:N)
 !CALL TAPER(PR,NFFT,width)
 !CALL TAPER(PI,NFFT,width)
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
 CALL TAPER(PR,NFFT,width)
 CALL TAPER(PI,NFFT,width)
 CALL KKFFT(PR,PI,NFFT,LOGN,FR,FI,1,0)
! Y(1:NLEN1-1)=FR(NFFT-NLEN1+2:NFFT)
! Y(NLEN1:NLEN1+NLEN2-1)=FR(1:NLEN2)
 DO I=1,M-1
  Y(I)=FR(NFFT-M+I+1)
 ENDDO
 DO I=1,N
  Y(M+I-1)=FR(I)
 ENDDO
 DEALLOCATE(PR,PI,FR,FI)
 RETURN
END SUBROUTINE


SUBROUTINE SCORF(A,NA,B,NB,CR,CI,NFFT,LOGN,width)
 IMPLICIT NONE
! THIS PROGRAM IS PURPOSED TO CALCULATE THE AUTO-CORRELATION OR
! CORSS-CORRELATION FUNCTION IN FREQUENCE-DOMAIN(FORM SAC2000). CREATIED: 2011-01-04 20:20:31
 INTEGER*4::I,J,NA,NB,LOGN,NFFT
 REAL*4::A(NA),B(NB),CR(NFFT),CI(NFFT),PR(NFFT),PI(NFFT),FR(NFFT),FI(NFFT),XR,XI,YR,YI,width
 PR=0.0E0
 PI=0.0E0
 FR=0.0E0
 FI=0.0E0
 PR(1:NA)=A(1:NA)
 PI(1:NB)=B(1:NB)
 CALL TAPER(PR,NFFT,width)
 CALL TAPER(PI,NFFT,width)
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
 CR=PR
 CI=PI
 RETURN
END SUBROUTINE


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








