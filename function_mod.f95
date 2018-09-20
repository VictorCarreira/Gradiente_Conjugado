MODULE functions
IMPLICIT NONE
  PUBLIC
  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)
  INTEGER(KIND=SP):: n
  REAL(KIND=DP)::inicio,final, TM, c
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:):: x, y
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: A, bt, xt

  
CONTAINS

REAL FUNCTION vec(A,bt,c,x,xt,n,y)

INTEGER,INTENT(IN):: n
INTEGER:: i,j 
REAL, INTENT(IN):: c
REAL,DIMENSION(:), ALLOCATABLE, INTENT(IN)::  x, xt 
REAL, DIMENSION(:,:), ALLOCATABLE, INTENT(IN):: A,bt
REAL, DIMENSION(:), ALLOCATABLE, INTENT(OUT):: y 

!ALLOCATE(b(2),bt(n,n),x(n),xt(n,n),y(n))

  DO i=1,n
   DO j=1,n      
     y(j)=((xt(j))/2)*A(i,j)*x(j)-bt(i,j)*x(i)+c
   END DO 
  END DO   

END FUNCTION vec

END MODULE functions