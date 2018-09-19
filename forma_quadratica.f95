PROGRAM forma_quadratica

    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
    !Programa de cálculo da forma quadrática                                     !
    !Autor: Victor Ribeiro Carreira                                              !
    !Categoria: Métodos Computacionais                                           !
    !Para usar compilação com flags utilize:                                     !
    !gfortran -fbounds-check -fbacktrace -Wall -Wextra -pedantic                 !
    !"pasta/subpasta/nomedopragrama.f95" -o nomedoexecutável                     !
    !++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!

                          !***************TABELA DE VARIÁVEIS**************!
                          !A: matriz quadrada, simétrica, positiva-definida!
                          !b: vetor conhecido                              !
                          !                                                !
                          !------------------------------------------------!


                !***************** PROBLEMA ********************!
                ! y=(xT)/2 Ax-bTx+c

 IMPLICIT NONE

  INTEGER, PARAMETER::SP = SELECTED_INT_KIND(r=8)
  INTEGER, PARAMETER::DP = SELECTED_REAL_KIND(12,100)

  INTEGER(KIND=SP):: i,j

  REAL(KIND=DP)::inicio,final, TM, c

  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:):: b, bT, x, xT, y
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: A
  
  CALL CPU_TIME(inicio)


  
  ALLOCATE(A(2,2),b(2),bT(10),x(10),xT(10),y(10))
  

   !OPEN(1,FILE='entrada.txt')
   OPEN(2,FILE='saida.txt')

   !A= 0.0d0
   
   !Preenchendo a matriz
   A(1,1)=3.0
   A(2,1)=2.0
   A(1,2)=2.0
   A(2,2)=6.0

   !preenchendo os vetores
   b(1)=2.0
   b(2)=-8.0

   ! preenchendo o escalar

   c=0.0

   WRITE(*,FMT=*)"A=",A
   WRITE(*,FMT=*)"b=",b

 
   xT = CALL transposta(10,A)
   
   FUNCTION vec(A,b,bT,c,x,xT,y)

      REAL(KIND=DP), INTENT(IN):: c
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE, INTENT(IN):: b, bT, x, xT 
      REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE, INTENT(IN):: A 
      REAL(KIND=DP), DIMENSION(:), ALLOCATABLE, INTENT(OUT):: y 
    
        y=((xT)/2)*A*x-bT*x+c  
   END FUNCTION vec

  CALL CPU_TIME(final)

  TM=final-inicio

  WRITE(*,*)'Tempo de máquina(s)=',TM

  CONTAINS

  SUBROUTINE transposta(n,A)
  IMPLICIT NONE
  INTEGER(KIND=SP), INTENT(IN):: n  !n, dimensão da matriz
  INTEGER(KIND=SP):: i, j
  REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT):: A !A é a matriz de entrada. Ela vai ser reescrita no processo

  ALLOCATE(A(n,n))

    DO i=1,n
      DO j=1,n
        A(j,i) = A(i,j)
      ENDDO
    ENDDO

END SUBROUTINE transposta


END PROGRAM forma_quadratica

