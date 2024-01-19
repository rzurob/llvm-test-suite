!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 22, 2007
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
!*  -- The implicit statement
!*  tests its scope
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
    TYPE(DT2(K0,L0,K1,L0,K2,L2)), POINTER :: Ptr=>NULL()
  END TYPE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT0(1,*)) :: Arg
  INTEGER ModFun
    ModFun = Arg%L0
  END FUNCTION

  END MODULE

  PROGRAM dtpImplicit2
  USE M

  IMPLICIT TYPE(DT0(1,3))(R)
  IMPLICIT TYPE(DT1(1,3,4,5))(S)
  IMPLICIT TYPE(DT2(1,3,4,5,8,7))(T)

  TARGET :: RT
  TARGET :: ST
  TARGET :: TT

  IF ( SIZE( ST%R )      .NE. S%L1         ) ERROR STOP 12
  IF ( ST%R%KIND         .NE. S%K1         ) ERROR STOP 13
  IF ( ANY ( ST%R        .NE. S%K1       ) ) ERROR STOP 14

  IF ( SIZE( TT%R )      .NE. T%L1         ) ERROR STOP 21
  IF ( TT%R%KIND         .NE. T%K1         ) ERROR STOP 22
  IF ( ANY ( TT%R        .NE. T%K1       ) ) ERROR STOP 23
  IF ( SIZE( TT%I )      .NE. T%L2         ) ERROR STOP 24
  IF ( TT%I%KIND         .NE. T%K2         ) ERROR STOP 25
  IF ( ANY ( TT%I        .NE. T%K2       ) ) ERROR STOP 26
  IF ( SIZE( TT%C )      .NE. T%L2         ) ERROR STOP 27
  IF ( TT%C%LEN          .NE. T%L2         ) ERROR STOP 28
  IF ( ANY (TT%C         .NE. CHAR(T%K2) ) ) ERROR STOP 29

  CALL IntSub()

  CONTAINS

  SUBROUTINE IntSub()
  INTEGER :: DT0, DT1, DT2

  POINTER :: RP, SP, TP

  RP => RT
  SP => ST
  TP => TT

  IF ( RP%ModFun()       .NE. RP%L0        ) ERROR STOP 31

  IF ( SIZE( SP%R )      .NE. S%L1         ) ERROR STOP 32
  IF ( SP%R%KIND         .NE. S%K1         ) ERROR STOP 33
  IF ( ANY ( SP%R        .NE. S%K1       ) ) ERROR STOP 34

  IF ( SIZE( TP%R )      .NE. T%L1         ) ERROR STOP 41
  IF ( TP%R%KIND         .NE. T%K1         ) ERROR STOP 42
  IF ( ANY ( TP%R        .NE. T%K1       ) ) ERROR STOP 43
  IF ( SIZE( TP%I )      .NE. T%L2         ) ERROR STOP 44
  IF ( TP%I%KIND         .NE. T%K2         ) ERROR STOP 45
  IF ( ANY ( TP%I        .NE. T%K2       ) ) ERROR STOP 46
  IF ( SIZE( TP%C )      .NE. T%L2         ) ERROR STOP 47
  IF ( TP%C%LEN          .NE. T%L2         ) ERROR STOP 48
  IF ( ANY (TP%C         .NE. CHAR(T%K2) ) ) ERROR STOP 49



  END SUBROUTINE

  END

