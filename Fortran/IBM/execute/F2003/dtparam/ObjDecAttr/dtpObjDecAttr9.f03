!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 28, 2007
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
!*  The DIMENSION attribute --  The assumed size array
!*
!*  -- The appearance of the whole assumed size array.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=0
    INTEGER, LEN  :: L0=0
    CHARACTER(L0) :: C0=""
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER, KIND :: K1=K0
    INTEGER, LEN  :: L1=K0
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K0), KIND :: K2=K0
    INTEGER(K0), LEN  :: L2=K0
    CHARACTER(L0), PUBLIC,   ALLOCATABLE  :: C(:)
    TYPE(DT0(K2,L2))                      :: X0
    TYPE(DT0(K2,L2)),        POINTER      :: X1
    TYPE(DT0(K2,L2)),        ALLOCATABLE  :: X2
    INTEGER(K1),   ALLOCATABLE  :: I(:)
  END TYPE

  TYPE(DT0),                DIMENSION(:), POINTER     :: T1
  TYPE(DT2(2,2)),           DIMENSION(:), ALLOCATABLE :: T2
  TYPE(DT2(1,1,K2=2,L2=:)), DIMENSION(:), POINTER     :: T3

  CONTAINS

  SUBROUTINE Change0(N, L, R, P1, P2, P3)
  INTEGER :: N, L, R, M
  TYPE(DT0),                DIMENSION(R:*) :: P1
  TYPE(DT2(2,2)),           DIMENSION(R:*) :: P2
  TYPE(DT2(1,1,K2=2,L2=L)), DIMENSION(R:*) :: P3
  INTEGER :: I, J

  M = R
  R = -R

  ! Pass the assumed size array
  CALL Change1(N, 2, M, P1, P2, P3)

  END SUBROUTINE

  SUBROUTINE Change1(N, L, R, P1, P2, P3)
  INTEGER :: N, L, R, M
  TYPE(DT0),                DIMENSION(R:*) :: P1
  TYPE(DT2(2,2)),           DIMENSION(R:*) :: P2
  TYPE(DT2(1,1,K2=2,L2=L)), DIMENSION(R:*) :: P3
  INTEGER :: I, J

  M = R
  R = -R

  IF ( ANY( LBOUND(P1) .NE. M )) ERROR STOP 80
  IF ( ANY( LBOUND(P2) .NE. M )) ERROR STOP 81
  IF ( ANY( LBOUND(P3) .NE. M )) ERROR STOP 82

  DO I = M, N

    IF ( P1(I)%K0               .NE.   0          ) ERROR STOP 11
    IF ( P1(I)%L0               .NE.   0          ) ERROR STOP 12

    IF ( P2(I)%K0               .NE.   2          ) ERROR STOP 21
    IF ( P2(I)%L0               .NE.   2          ) ERROR STOP 22
    IF ( P2(I)%K1%KIND          .NE.   4          ) ERROR STOP 23
    IF ( P2(I)%L1%KIND          .NE.   4          ) ERROR STOP 24
    IF ( P2(I)%K1               .NE.   2          ) ERROR STOP 25
    IF ( P2(I)%L1               .NE.   2          ) ERROR STOP 26
    IF ( P2(I)%I%KIND           .NE.   2          ) ERROR STOP 27
    IF ( SIZE(P2(I)%I)          .NE.   1          ) ERROR STOP 28
    IF ( ANY(P2(I)%I            .NE.  -1        ) ) ERROR STOP 29
    IF ( P2(I)%C%LEN            .NE.   2          ) ERROR STOP 30
    IF ( SIZE(P2(I)%C)          .NE.   1          ) ERROR STOP 31
    IF ( ANY(P2(I)%C            .NE.   "X"      ) ) ERROR STOP 32

    IF ( P3(I)%K0               .NE.   1          ) ERROR STOP 41
    IF ( P3(I)%L0               .NE.   1          ) ERROR STOP 42
    IF ( P3(I)%K1%KIND          .NE.   4          ) ERROR STOP 43
    IF ( P3(I)%L1%KIND          .NE.   4          ) ERROR STOP 44
    IF ( P3(I)%K1               .NE.   1          ) ERROR STOP 45
    IF ( P3(I)%L1               .NE.   1          ) ERROR STOP 46
    IF ( P3(I)%I%KIND           .NE.   1          ) ERROR STOP 47
    IF ( SIZE(P3(I)%I)          .NE.   2          ) ERROR STOP 48
    IF ( ANY(P3(I)%I            .NE.  -1        ) ) ERROR STOP 49
    IF ( P3(I)%C%LEN            .NE.   1          ) ERROR STOP 50
    IF ( SIZE(P3(I)%C)          .NE.   2          ) ERROR STOP 51
    IF ( ANY(P3(I)%C            .NE.   "X"      ) ) ERROR STOP 52

    IF ( P3(I)%X0%K0            .NE.   2          ) ERROR STOP 61
    IF ( P3(I)%X0%L0            .NE.   2          ) ERROR STOP 62
    IF ( P3(I)%X1%K0            .NE.   2          ) ERROR STOP 61
    IF ( P3(I)%X1%L0            .NE.   2          ) ERROR STOP 62
    IF ( P3(I)%X2%K0            .NE.   2          ) ERROR STOP 61
    IF ( P3(I)%X2%L0            .NE.   2          ) ERROR STOP 62

  END DO

  END SUBROUTINE

  END MODULE

  PROGRAM dtpObjDecAttr9
  USE M

  INTEGER  :: N = 107, R=62, I

  ALLOCATE(T1(-N:-1), SOURCE=DT0())
  ALLOCATE(T2(-N:-1), SOURCE=DT2(2,2)(I=[-1], C=["X"], X0=DT0(2,2)(), X1=NULL(), X2=NULL()))
  ALLOCATE(T3(-N:-1), SOURCE=DT2(1,1,K2=2,L2=2)(I=[-1,-1],C=["X","X"], X0=DT0(2,2)(), X1=NULL(), X2=NULL()))

  ! Start at the R_th element
  CALL Change0(N, 2, R, T1(-(N-R+1):-1), T2(-(N-R+1):-1), T3(-(N-R+1):-1))

  END
