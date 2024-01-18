!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 23, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  The DIMENSION attribute --  The deferred shape array
!*  The bounds of the array target or allocatable array are unaffected by any
!*  subsequent redefinition or undefinition of variables involved in the bounds'
!*  specification expressions.
!*  (ICE)
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
    INTEGER(K1),   ALLOCATABLE  :: I(:)
    CHARACTER(L0), PUBLIC,   ALLOCATABLE  :: C(:)
  END TYPE

  TYPE(DT0),          DIMENSION(:,:), ALLOCATABLE :: T1
  TYPE(DT1(2,2)),     DIMENSION(:,:), POINTER     :: T2
  TYPE(DT1(1,1,2,:)), DIMENSION(:,:), ALLOCATABLE :: T3

  CONTAINS

  SUBROUTINE Change(N, L, P1, P2, P3)
  INTEGER N, L, M
  TYPE(DT0),                DIMENSION(-N:-1,N:2*N-1) :: P1
  TYPE(DT1(2,2)),           DIMENSION(-N:-1,N:2*N-1) :: P2
  TYPE(DT1(1,1,K1=2,L1=L)), DIMENSION(-N:-1,N:2*N-1) :: P3
  INTEGER :: I, J

  M = N
  N = -N + 1
  L = -L + 1

  IF ( ANY( LBOUND(P1) .NE. [-M, m] )) STOP 60
  IF ( ANY( LBOUND(P2) .NE. [-M, m] )) STOP 61
  IF ( ANY( LBOUND(P3) .NE. [-M, m] )) STOP 62

  IF ( SIZE( P1 ) .NE. m*m ) STOP 70
  IF ( SIZE( P2 ) .NE. m*m ) STOP 71
  IF ( SIZE( P3 ) .NE. m*m ) STOP 72

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

  END DO
  END DO

  END SUBROUTINE

  END MODULE

  PROGRAM dtpObjDecAttr6
  USE M
  implicit none
  integer, parameter :: n = 107

  ALLOCATE(T1(-N:-1,N), SOURCE=DT0())
  ALLOCATE(T2(-N:-1,N), SOURCE=DT1(2,2)(I=[-1], C=["X"]))
  ALLOCATE(T3(-N:-1,N), SOURCE=DT1(1,1,2,2)(I=[-1,-1],C=["X","X"]))

  CALL Change(n, 2, T1, T2, T3)

  END

