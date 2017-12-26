!This program is purposed to calculate the auto-correlation or 
!corss-correlation function
!creatied 2010.05.13   

   SUBROUTINE COR(X,M,H,N,Y)
   REAL::X(M),H(N),Y(M+N-1)
!注意X(M),H(N)为输入，Y(M+N-1)为X(M)与H(N)自相关序列
   DO 10 K=1,M
      Y(K)=0.0
      DO 20 I=1,N
         IF(K+I.LE.M+1) Y(K)=Y(K)+H(I)*X(K+I-1)
      !注意书上的公式似乎错了
20    CONTINUE
10 CONTINUE
   END
