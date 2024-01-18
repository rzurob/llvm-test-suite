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
!*
!*  ()
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
    INTEGER(K1),   PRIVATE,  ALLOCATABLE  :: I(:)
    CHARACTER(L0), PUBLIC,   ALLOCATABLE  :: C(:)
  END TYPE

  CONTAINS

  SUBROUTINE GetAllocated(N,L, T1, T2, T3)
  INTEGER N,L

  TYPE(DT0),          DIMENSION(:,:), ALLOCATABLE :: T1
  TYPE(DT1(2,2)),     DIMENSION(:,:), POINTER     :: T2
  TYPE(DT1(1,1,2,L)), DIMENSION(:,:), ALLOCATABLE :: T3

  DEALLOCATE(T1,T2,T3)

  ALLOCATE(T1(-N:-1,N), SOURCE=DT0())
  ALLOCATE(T2(-N:-1,N), SOURCE=DT1(2,2)(I=[-1], C=["X"]))
  ALLOCATE(T3(-N:-1,N), SOURCE=DT1(1,1,2,2)(I=[-1,-1],C=["X","X"]))

  END SUBROUTINE

  SUBROUTINE Check(N, L)
  INTEGER N, L
  TYPE(DT0),                DIMENSION(:,:), ALLOCATABLE :: P1
  TYPE(DT1(2,2)),           DIMENSION(:,:), POINTER     :: P2
  TYPE(DT1(1,1,K1=2,L1=L)), DIMENSION(:,:), ALLOCATABLE :: P3
  INTEGER :: I, J

  ALLOCATE(P1(N:2*N-1,N:2*N-1), SOURCE=DT0())
  ALLOCATE(P2(-N:-1,N:2*N-1), SOURCE=DT1(2,2)(I=[-3], C=["?"]))
  ALLOCATE(P3(-N:-1,-N:-1), SOURCE=DT1(1,1,2,2)(I=[-4,-5],C=["?","?"]))

  CALL GetAllocated(N, L, P1, P2, P3)

  IF ( ANY( LBOUND(P1) .NE. [-N,1] )) STOP 60
  IF ( ANY( LBOUND(P2) .NE. [-N,1] )) STOP 61
  IF ( ANY( LBOUND(P3) .NE. [-N,1] )) STOP 62

  IF ( SIZE( P1 ) .NE. N*N ) STOP 70
  IF ( SIZE( P2 ) .NE. N*N ) STOP 71
  IF ( SIZE( P3 ) .NE. N*N ) STOP 72

  DO I = -N ,-1
  DO J = 1, N

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

  PROGRAM dtpObjDecAttr5
  USE M

  CALL Check(107, 2)

  END

