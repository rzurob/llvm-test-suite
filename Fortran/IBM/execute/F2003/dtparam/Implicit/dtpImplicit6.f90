!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpImplicit6
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
!*  polymorphism -- deferred length parameter
!*
!*  (338494)
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
    IMPLICIT CLASS(DT0(1,*))(R)
    IMPLICIT CLASS(DT1(1,*,4,*))(S)
    IMPLICIT CLASS(DT2(1,*,4,*,8,*))(T)
    INTEGER   :: N
    DIMENSION :: R(:)
    DIMENSION :: S(:)
    DIMENSION :: T(:)
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

  PROGRAM dtpImplicit6
  USE M

  IMPLICIT CLASS(DT0(1,:))(R)
  IMPLICIT CLASS(DT1(1,:,4,:))(S)
  IMPLICIT CLASS(DT2(1,:,4,:,8,:))(T)
  TYPE(DT0(1,3)),        TARGET :: RT(97)
  TYPE(DT1(1,3,4,5)),    TARGET :: ST(97)
  TYPE(DT2(1,3,4,5,8,7)),TARGET :: TT(97)
  POINTER                       :: SP(:), RP(:),  TP(:)
  INTEGER :: I

  RP(-97:) => RT
  SP(-97:) => ST
  TP(-97:) => TT

  DO I=1, 97

    IF ( SIZE( ST(I)%R )      .NE. S%L1         ) STOP 12
    IF ( ST(I)%R%KIND         .NE. S%K1         ) STOP 13
    IF ( ANY ( ST(I)%R        .NE. S%K1       ) ) STOP 14

    IF ( SIZE( TT(I)%R )      .NE. T%L1         ) STOP 21
    IF ( TT(I)%R%KIND         .NE. T%K1         ) STOP 22
    IF ( ANY ( TT(I)%R        .NE. T%K1       ) ) STOP 23
    IF ( SIZE( TT(I)%I )      .NE. T%L2         ) STOP 24
    IF ( TT(I)%I%KIND         .NE. T%K2         ) STOP 25
    IF ( ANY ( TT(I)%I        .NE. T%K2       ) ) STOP 26
    IF ( SIZE( TT(I)%C )      .NE. T%L2         ) STOP 27
    IF ( TT(I)%C%LEN          .NE. T%L2         ) STOP 28
    IF ( ANY (TT(I)%C         .NE. CHAR(T%K2) ) ) STOP 29

  END DO

  CALL ExtSub(RP, SP, TP, 97)

  END

  SUBROUTINE ExtSub(RP,SP,TP, N)
  USE M, ONLY: DT0,DT1,DT2,R,S,T

  IMPLICIT CLASS(DT0(1,*))(R)
  IMPLICIT CLASS(DT1(1,*,4,*))(S)
  IMPLICIT CLASS(DT2(1,*,4,*,8,*))(T)
  INTEGER   :: N, I
  DIMENSION :: RP(:)
  DIMENSION :: SP(:)
  DIMENSION :: TP(:)

  DO I=1, N

    IF ( RP(I)%ModFun()       .NE. RP%L0        ) STOP 31

    IF ( SIZE( SP(I)%R )      .NE. S%L1         ) STOP 32
    IF ( SP(I)%R%KIND         .NE. S%K1         ) STOP 33
    IF ( ANY ( SP(I)%R        .NE. S%K1       ) ) STOP 34

    IF ( SIZE( TP(I)%R )      .NE. T%L1         ) STOP 41
    IF ( TP(I)%R%KIND         .NE. T%K1         ) STOP 42
    IF ( ANY ( TP(I)%R        .NE. T%K1       ) ) STOP 43
    IF ( SIZE( TP(I)%I )      .NE. T%L2         ) STOP 44
    IF ( TP(I)%I%KIND         .NE. T%K2         ) STOP 45
    IF ( ANY ( TP(I)%I        .NE. T%K2       ) ) STOP 46
    IF ( SIZE( TP(I)%C )      .NE. T%L2         ) STOP 47
    IF ( TP(I)%C%LEN          .NE. T%L2         ) STOP 48
    IF ( ANY (TP(I)%C         .NE. CHAR(T%K2) ) ) STOP 49

  END DO

  END SUBROUTINE

