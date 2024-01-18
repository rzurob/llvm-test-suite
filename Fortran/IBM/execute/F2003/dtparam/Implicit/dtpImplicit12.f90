!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpImplicit12
!*
!*  DATE                       : Jun. 27, 2007
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
!*   on associate
!*
!*  (ICE)
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

  TYPE(DT0(1,3))         :: R(97)
  TYPE(DT1(1,3,4,5))     :: S(97)
  TYPE(DT2(1,3,4,5,8,7)) :: T(97)
  SAVE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT0(1,*)) :: Arg
  INTEGER ModFun
    ModFun = -Arg%L0
  END FUNCTION

  END MODULE

  PROGRAM dtpImplicit12
  USE M

  IMPLICIT TYPE(DT0(1,3))(R)
  IMPLICIT TYPE(DT1(1,3,4,5))(S)
  IMPLICIT TYPE(DT2(1,3,4,5,8,7))(T)

  ASSOCIATE (RA => r(1) )
  ASSOCIATE (SA => s(1) )
  ASSOCIATE (TA => t(1))

    IF ( RA%ModFun()       .NE. -R%L0        ) STOP 31

    IF ( SIZE( SA%R )      .NE. S%L1         ) STOP 32
    IF ( SA%R%KIND         .NE. S%K1         ) STOP 33
    IF ( ANY ( SA%R        .NE. S%K1       ) ) STOP 34

    IF ( SIZE( TA%R )      .NE. T%L1         ) STOP 41
    IF ( TA%R%KIND         .NE. T%K1         ) STOP 42
    IF ( ANY ( TA%R        .NE. T%K1       ) ) STOP 43
    IF ( SIZE( TA%I )      .NE. T%L2         ) STOP 44
    IF ( TA%I%KIND         .NE. T%K2         ) STOP 45
    IF ( ANY ( TA%I        .NE. T%K2       ) ) STOP 46
    IF ( SIZE( TA%C )      .NE. T%L2         ) STOP 47
    IF ( TA%C%LEN          .NE. T%L2         ) STOP 48
    IF ( ANY (TA%C         .NE. CHAR(T%K2) ) ) STOP 49

  END ASSOCIATE
  END ASSOCIATE
  END ASSOCIATE

  END

