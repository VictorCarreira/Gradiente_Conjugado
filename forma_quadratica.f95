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

  INTEGER(KIND=SP):: n

  REAL(KIND=DP)::inicio,final, TM, c

  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:):: b, x, y
  REAL(KIND=DP), ALLOCATABLE, DIMENSION(:,:):: A, bt, xt
  
  CALL CPU_TIME(inicio)

  n=10
  
  ALLOCATE(A(2,2),b(2),bt(n,n),x(n),xt(n,n),y(n))
  

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

  CALL transpostaV(n,bt)

  CALL transpostaV(n,xt)

  CALL vec(A,bt,b,c,x,xt,n,y)

  WRITE(*,*)'y=',y
    
  CALL CPU_TIME(final)

  TM=final-inicio

  WRITE(*,*)'Tempo de máquina(s)=',TM

  CONTAINS

  SUBROUTINE transpostaM(n,A)
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

END SUBROUTINE transpostaM

SUBROUTINE transpostaV(n,At)
IMPLICIT NONE
INTEGER(KIND=SP), INTENT(IN):: n  !n, dimensão da matriz
INTEGER(KIND=SP):: i
REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT):: At !A é a matriz de entrada. Ela vai ser reescrita no processo
REAL(KIND=DP), DIMENSION(:,:), ALLOCATABLE:: A

ALLOCATE(A(1,n),At(n,1))

  DO i=1,n
      At(:,i) = A(i,:)
  ENDDO

END SUBROUTINE transpostaV

END PROGRAM forma_quadratica
