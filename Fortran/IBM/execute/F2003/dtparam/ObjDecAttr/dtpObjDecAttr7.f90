!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr7
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 24, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!* 
!*  The DIMENSION attribute --  The assumed size array
!* 
!*  
!* 
!*  (complaint on 130)
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=0
    INTEGER, LEN  :: L0=0
  END TYPE

  TYPE, EXTENDS(DT0) :: DT1(K1,L1)
    INTEGER(K0), KIND :: K1=2*K0
    INTEGER(K0), LEN  :: L1=2*K0
    CHARACTER(L0), PUBLIC,   ALLOCATABLE  :: C(:)
    TYPE(DT0(K1,L1))                :: X0
    TYPE(DT0(K1,L1)), POINTER       :: X1
    TYPE(DT0(K1,L1)), ALLOCATABLE   :: X2
    INTEGER(K1),   ALLOCATABLE  :: I(:)
  END TYPE

  TYPE(DT0),          DIMENSION(:,:), POINTER     :: T1
  TYPE(DT1(2,2)),     DIMENSION(:,:), ALLOCATABLE :: T2
  TYPE(DT1(1,1,2,:)), DIMENSION(:,:), POINTER     :: T3

  CONTAINS

  SUBROUTINE Change(N, L, P1, P2, P3)
  INTEGER N, L, M
  TYPE(DT0),                DIMENSION(-N:-1,N:*) :: P1
  TYPE(DT1(2,2)),           DIMENSION(-N:-1,N:*) :: P2
  TYPE(DT1(1,1,K1=2,L1=L)), DIMENSION(-N:-1,N:*) :: P3
  INTEGER :: I, J

  M = N
  N = -N + 1
  L = -L + 1
 
  IF ( ANY( LBOUND(P1) .NE. [-M, m] )) STOP 60
  IF ( ANY( LBOUND(P2) .NE. [-M, m] )) STOP 61
  IF ( ANY( LBOUND(P3) .NE. [-M, m] )) STOP 62

  DO I = -M, -1 
  DO J = M, 2*M-1 

    IF ( P1(I,J)%K0               .NE.   0          ) STOP 11
    IF ( P1(I,J)%L0               .NE.   0          ) STOP 12

    IF ( P2(I,J)%K0               .NE.   2          ) STOP 21
    IF ( P2(I,J)%L0               .NE.   2          ) STOP 22
    IF ( P2(I,J)%K1%KIND          .NE.   2          ) STOP 23
    IF ( P2(I,J)%L1%KIND          .NE.   2          ) STOP 24
    IF ( P2(I,J)%K1               .NE.   4          ) STOP 25
    IF ( P2(I,J)%L1               .NE.   4          ) STOP 26
    IF ( P2(I,J)%I%KIND           .NE.   4          ) STOP 27
    IF ( SIZE(P2(I,J)%I)          .NE.   1          ) STOP 28
    IF ( ANY(P2(I,J)%I            .NE.  -1        ) ) STOP 29
    IF ( P2(I,J)%C%LEN            .NE.   2          ) STOP 30
    IF ( SIZE(P2(I,J)%C)          .NE.   1          ) STOP 31
    IF ( ANY(P2(I,J)%C            .NE.   "X"      ) ) STOP 32

    IF ( P3(I,J)%K0               .NE.   1          ) STOP 41
    IF ( P3(I,J)%L0               .NE.   1          ) STOP 42
    IF ( P3(I,J)%K1%KIND          .NE.   1          ) STOP 43
    IF ( P3(I,J)%L1%KIND          .NE.   1          ) STOP 44
    IF ( P3(I,J)%K1               .NE.   2          ) STOP 45
    IF ( P3(I,J)%L1               .NE.   2          ) STOP 46
    IF ( P3(I,J)%I%KIND           .NE.   2          ) STOP 47
    IF ( SIZE(P3(I,J)%I)          .NE.   2          ) STOP 48
    IF ( ANY(P3(I,J)%I            .NE.  -1        ) ) STOP 49
    IF ( P3(I,J)%C%LEN            .NE.   1          ) STOP 50
    IF ( SIZE(P3(I,J)%C)          .NE.   2          ) STOP 51
    IF ( ANY(P3(I,J)%C            .NE.   "X"      ) ) STOP 52

    IF ( P3(I,J)%X0%K0            .NE.   2          ) STOP 61
    IF ( P3(I,J)%X0%L0            .NE.   2          ) STOP 62


  END DO
  END DO

  END SUBROUTINE

  END MODULE

  PROGRAM dtpObjDecAttr7
  USE M
 
  INTEGER  :: N = 107
 
  ALLOCATE(T1(-N:-1,N), SOURCE=DT0())
  ALLOCATE(T2(-N:-1,N), SOURCE=DT1(2,2)(I=[-1], C=["X"], X0=DT0(4,4)(), X1=NULL(), X2=NULL()))
  ALLOCATE(T3(-N:-1,N), SOURCE=DT1(1,1,2,2)(I=[-1,-1],C=["X","X"], X0=DT0(2,2)(), X1=NULL(), X2=NULL()))

  CALL Change(N, 2, T1, T2, T3) 

  END

