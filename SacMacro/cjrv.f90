program readrv
 implicit none
 
 integer*4::i,j,k,m,n
 real*4::endtp,tp,null
 real*4,allocate::t(:),v(:)
 
 
 endtp=200.0
 i=1
 open(10,file='SREGN.ASC',status='old')
 read(10,*)
 do while(tp.le.endtp)
  read(10,*) null,null,tp
  i=i+1
 enddo
 rewind(10)
 read(10,*)
 do j=1,i
  read(10,*) null,null,t(:),null,v(:)
 enddo
  
 