!本文件存放一些未完成的有用子程序


subroutine sfft(Xreal,Ximag,nfft,logn,idir)
! Compute the Fast Fourier Transform (FFT) of a sequence.
! nfft : Length of arrays xreal and ximag (nfft=2**logn)
! logn  Base-2 Logarithm of n
! idir : Direction of the Transform
!        -1 Forward Transform
!         1 Inverse Transform (Normalization performed here)
! The transform result is retured in xreal and ximag
! and the initial sequence is destroyed
 implicit none
 integer*4::i,j,k,nfft,logn,nby2
 integer*4::nm1,nblock,irel,nstage,iblock,iti,icount,i1
 real*4::temp,sine,cosine,scale
 real*4::Xreal(nfft),Ximag(nfft),Tcos(nfft),Tsin(nfft)
 real*8::dcosn,dsine,fund
! TABLE GENERATOR
 nby2=nfft/2
 fund=3.141592653589793d0*2.0d0
 fund=fund/nfft
 dcosn=cos(fund)
 dsine=sin(fund)
 if(idir.eq.-1) dsine=-dsine
 Tcos(1)=1.0e0
 Tsin(1)=0.0e0
 do i=2,nby2
  Tcos(i)=dcosn*Tcos(i-1)-dsine*Tsin(i-1)
  Tsin(i)=dcosn*Tsin(i-1)+dsine*Tcos(i-1)
 enddo
! BIT REVERSE CODE
 nm1=nfft-1
 j=1
 do i=1,nm1
  if(i.lt.j) then
   temp=Xreal(i)
   Xreal(i)=Xreal(j)
   Xreal(j)=temp
   temp=Ximag(i)
   Ximag(i)=Ximag(j)
   Ximag(j)=temp
  endif
  k=nby2
  do while(k.lt.j)
   j=j-k
   k=k/2
  enddo
  j=j+k
 enddo
! INDEXING CODE
 nblock=nfft
 irel=1
 do nstage=1,logn
  if(nstage.gt.1) irel=irel*2
  i=-irel
  nblock=nblock/2
  do iblock=1,nblock
   i=i+irel
   iti=1-nblock
   do icount=1,irel
    i=i+1
    iti=iti+nblock
    i1=i+irel
! BUTTERFLY CODE
    if(nstage.gt.1 ) then
     sine=Tsin(iti)
		 cosine=Tcos(iti)
		 temp=Xreal(i1)*cosine-Ximag(i1)*sine
		 Ximag(i1)=Xreal(i1)*sine+Ximag(i1)*cosine
		 Xreal(i1)=temp
    endif
    temp=Xreal(i)+Xreal(i1)
    Xreal(i1)=Xreal(i)-Xreal(i1)
    Xreal(i)=temp
    temp=Ximag(i)+Ximag(i1)
    Ximag(i1)=Ximag(i)-Ximag(i1)
    Ximag(i)=temp
   enddo
  enddo
 enddo
! IF REVERSE TRANSFORM, DIVIDE THROUGH BY N
 if(idir.eq.1) then
  scale=1.0e0/nfft
  do i=1,nfft
   Xreal(i)=Xreal(i)*scale
   Ximag(i)=Ximag(i)*scale
  enddo
 endif
 return
end subroutine


subroutine crscor(data1,npts1,data2,npts2,out,nfft)
! Compute the Cross-Correlation Function
 implicit none
 integer*4::i,j,k,npts1,npts2,nsamps,nfft,half
 real*4::data1(npts1),data2(npts2),xi,xr,yi,yr
 real*4,allocatable::out(:),workr(:),worki(:)
! Find first power of two
 nsamps=max(npts1,npts2)
 k=floor(log10(2*nsamps-1.0e0)/log10(2.0e0))+1
 nfft=2**k
 half=nfft/2
! Compute cross-correlation function
! Initialize correlation arrays
 allocate(out(nfft))
 allocate(caux(nfft))
 allocate(workr(nfft))
 allocate(worki(nfft))
 out=0.0e0
 caux=0.0e0
 workr=0.0e0
 worki=0.0e0
 workr(1:npts1)=data1(1:npts1)
 worki(1:npts1)=data2(1:npts2)
 call sfft(workr,worki,nfft,-1)
 out(1)=workr(1)*worki(1)
 do i=2,half+1
  j=nfft-i+2
  xr=(workr(i)+workr(j))/2
  xi=(worki(i)-worki(j))/2
  yr=(worki(i)+worki(j))/2
  yi=(workr(j)-workr(i))/2
  c(i)=xr*yr + xi*yi
  caux(i)=xr*yi-xi*yr
  c(j)=c(i)
  caux(j)=-caux(i)
 enddo
 call sfft(out,caux,nfft,1)
!(缺，未完成)
 deallocate(out)
 deallocate(caux)
 deallocate(workr)
 deallocate(worki)
 return
end subroutine


SUBROUTINE FDNH(M,ND,ND1,PS,Z,N,B,Y,V,SGM,S,X)
!一般多项式分段曲线拟合——FORTRAN5.0实用算法汇编(高林,学苑出版社,1994)
! M:输入参数,整变量,拟合多项式项数(阶数加1)
! ND:输入参数,整变量,分段数
! ND1:输入参数,整变量,ND1=ND-1
! PS:输入参数,一维实数组,容量ND1,存放各分点的值
! Z:输入参数,二维实数组,容量N*2,存放观测数据序列
! N:输入参数,整变量,观测数据样点数
! B:输出参数,一维实数组,容量(M+ND1),存放全部拟合系数值
! Y,V:输出参数,一维实数组,容量N,存放拟合值及拟合残差值
! SGM:输出参数,实变量,拟合参差的标准差
! S:工作单元,二维实数组,容量(M+ND1)*(M+ND)
! X:工作单元,一维实数组,容量(M+ND1)
    implicit real*8 (A-H,P-Z)
    DIMENSION Z(N,2),PS(ND1),B(M+ND1),Y(N),V(N)
    DIMENSION X(M+ND1),S(M+ND1,M+ND)
    MND=M+ND
    MND1=M+ND1
    DO 10 I=1,MND1
     DO 20 J=1,MND
      S(I,J)=0.0
20   CONTINUE
10  CONTINUE
    DO 30 K=1,N
     T=Z(K)

END SUBROUTINE