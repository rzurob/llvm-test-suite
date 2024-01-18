!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 26, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED CLASS PARAMETERS
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
!*  -- The implicit statement
!*  polymorphism -- the mix of deferred and assumed type parameters
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    CONTAINS
    PROCEDURE :: ModFun
  END TYPE

  TYPE,  EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND :: K1=1
    INTEGER(K0), LEN  :: L1=1
    REAL(K1) :: R(L1)=K1
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND :: K2=1
    INTEGER(K1), LEN  :: L2=1
    CHARACTER(L2) :: C(L2)=CHAR(K2)
    INTEGER(K2)   :: I(L2)=K2
    CLASS(DT2(K0,L0,K1,L0,K2,L2)), POINTER :: Ptr=>NULL()
  END TYPE

  INTERFACE
    SUBROUTINE ExtSub(R,S,T, N)
    IMPORT
    IMPLICIT CLASS(DT0(1,:))(R)
    IMPLICIT CLASS(DT1(1,:,4,:))(S)
    IMPLICIT CLASS(DT2(1,:,4,:,8,:))(T)
    INTEGER     :: N
    ALLOCATABLE :: R(:)
    ALLOCATABLE :: S(:)
    ALLOCATABLE :: T(:)
    END SUBROUTINE
  END INTERFACE

  TYPE(DT0(1,3))         :: R(97)
  TYPE(DT1(1,3,4,5))     :: S(97)
  TYPE(DT2(1,3,4,5,8,7)) :: T(97)
  SAVE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT0(1,*)) :: Arg
  INTEGER ModFun
    ModFun = Arg%L0
  END FUNCTION

  END MODULE

  PROGRAM dtpImplicit7
  USE M

  IMPLICIT CLASS(DT0(1,:))(R)
  IMPLICIT CLASS(DT1(1,:,4,:))(S)
  IMPLICIT CLASS(DT2(1,:,4,:,8,:))(T)
  ALLOCATABLE :: RT(:)
  ALLOCATABLE :: ST(:)
  ALLOCATABLE :: TT(:)
  INTEGER :: I

  ALLOCATE(DT0(1,3) :: RT(97))
  ALLOCATE(DT1(1,3,4,5) :: ST(97))
  ALLOCATE(DT2(1,3,4,5,8,7) :: TT(97))

  DO I=1, 97

    IF ( RT(I)%L0             .NE. R%L0         ) ERROR STOP 11

    IF ( SIZE( ST(I)%R )      .NE. S%L1         ) ERROR STOP 12
    IF ( ST(I)%R%KIND         .NE. S%K1         ) ERROR STOP 13
    IF ( ANY ( ST(I)%R        .NE. S%K1       ) ) ERROR STOP 14

    IF ( SIZE( TT(I)%R )      .NE. T%L1         ) ERROR STOP 21
    IF ( TT(I)%R%KIND         .NE. T%K1         ) ERROR STOP 22
    IF ( ANY ( TT(I)%R        .NE. T%K1       ) ) ERROR STOP 23
    IF ( SIZE( TT(I)%I )      .NE. T%L2         ) ERROR STOP 24
    IF ( TT(I)%I%KIND         .NE. T%K2         ) ERROR STOP 25
    IF ( ANY ( TT(I)%I        .NE. T%K2       ) ) ERROR STOP 26
    IF ( SIZE( TT(I)%C )      .NE. T%L2         ) ERROR STOP 27
    IF ( TT(I)%C%LEN          .NE. T%L2         ) ERROR STOP 28
    IF ( ANY (TT(I)%C         .NE. CHAR(T%K2) ) ) ERROR STOP 29

  END DO

  CALL ExtSub(RT, ST, TT, 97)

  END

  SUBROUTINE ExtSub(RA,SA,TA, N)
  USE M, ONLY: DT0,DT1,DT2,R,S,T

  IMPLICIT CLASS(DT0(1,:))(R)
  IMPLICIT CLASS(DT1(1,:,4,:))(S)
  IMPLICIT CLASS(DT2(1,:,4,:,8,:))(T)
  INTEGER     :: N, I
  ALLOCATABLE :: RA(:)
  ALLOCATABLE :: SA(:)
  ALLOCATABLE :: TA(:)

  IF ( .NOT. ALLOCATED(RA) ) ERROR STOP 91
  IF ( .NOT. ALLOCATED(SA) ) ERROR STOP 92
  IF ( .NOT. ALLOCATED(TA) ) ERROR STOP 93

  DO I=1, N

    IF ( RA(I)%ModFun()       .NE. R%L0         ) ERROR STOP 31

    IF ( SIZE( SA(I)%R )      .NE. S%L1         ) ERROR STOP 32
    IF ( SA(I)%R%KIND         .NE. S%K1         ) ERROR STOP 33
    IF ( ANY ( SA(I)%R        .NE. S%K1       ) ) ERROR STOP 34

    IF ( SIZE( TA(I)%R )      .NE. T%L1         ) ERROR STOP 41
    IF ( TA(I)%R%KIND         .NE. T%K1         ) ERROR STOP 42
    IF ( ANY ( TA(I)%R        .NE. T%K1       ) ) ERROR STOP 43
    IF ( SIZE( TA(I)%I )      .NE. T%L2         ) ERROR STOP 44
    IF ( TA(I)%I%KIND         .NE. T%K2         ) ERROR STOP 45
    IF ( ANY ( TA(I)%I        .NE. T%K2       ) ) ERROR STOP 46
    IF ( SIZE( TA(I)%C )      .NE. T%L2         ) ERROR STOP 47
    IF ( TA(I)%C%LEN          .NE. T%L2         ) ERROR STOP 48
    IF ( ANY (TA(I)%C         .NE. CHAR(T%K2) ) ) ERROR STOP 49

  END DO

  END SUBROUTINE

