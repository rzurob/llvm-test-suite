!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 16, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type parameters
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
!*  Type param def stmt - kind selector
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K, L)
    INTEGER(KIND=1), KIND :: K
    INTEGER(KIND=K), LEN  :: L=K
    INTEGER(KIND=K)       :: KK=K
    INTEGER(KIND=K)       :: LL(L, L)=K
    CONTAINS
    GENERIC :: Proc => ModSub1, ModSub2, ModSub4, ModSub8
    PROCEDURE ::  ModSub1
    PROCEDURE ::  ModSub2
    PROCEDURE ::  ModSub4
    PROCEDURE ::  ModSub8
  END TYPE

  CONTAINS

  SUBROUTINE ModSub1(Arg)
  CLASS(DT(1,*)) :: Arg
    IF ( Arg%K             .NE. Arg%KK )          ERROR STOP 11
    IF ( Arg%K             .NE. KIND(Arg%KK) )    ERROR STOP 12
    IF ( KIND(Arg%LL)      .NE. Arg%K )           ERROR STOP 13
    IF ( ANY(SHAPE(Arg%LL) .NE. (/Arg%L, Arg%L/)))ERROR STOP 14
    IF ( ANY(Arg%LL        .NE. Arg%K) )          ERROR STOP 15
  END SUBROUTINE

  SUBROUTINE ModSub2(Arg)
  CLASS(DT(2,*)) :: Arg
    IF ( Arg%K             .NE. Arg%KK )          ERROR STOP 21
    IF ( Arg%K             .NE. KIND(Arg%KK) )    ERROR STOP 22
    IF ( KIND(Arg%LL)      .NE. Arg%K )           ERROR STOP 23
    IF ( ANY(SHAPE(Arg%LL) .NE. (/Arg%L, Arg%L/)))ERROR STOP 24
    IF ( ANY(Arg%LL        .NE. Arg%K) )          ERROR STOP 25
  END SUBROUTINE

  SUBROUTINE ModSub4(Arg)
  CLASS(DT(4,*)) :: Arg
    IF ( Arg%K             .NE. Arg%KK )          ERROR STOP 41
    IF ( Arg%K             .NE. KIND(Arg%KK) )    ERROR STOP 42
    IF ( KIND(Arg%LL)      .NE. Arg%K )           ERROR STOP 43
    IF ( ANY(SHAPE(Arg%LL) .NE. (/Arg%L, Arg%L/)))ERROR STOP 44
    IF ( ANY(Arg%LL        .NE. Arg%K) )          ERROR STOP 45
  END SUBROUTINE

  SUBROUTINE ModSub8(Arg)
  CLASS(DT(8,*)) :: Arg
    IF ( Arg%K             .NE. Arg%KK )          ERROR STOP 81
    IF ( Arg%K             .NE. KIND(Arg%KK) )    ERROR STOP 82
    IF ( KIND(Arg%LL)      .NE. Arg%K )           ERROR STOP 83
    IF ( ANY(SHAPE(Arg%LL) .NE. (/Arg%L, Arg%L/)))ERROR STOP 84
    IF ( ANY(Arg%LL        .NE. Arg%K) )          ERROR STOP 85
  END SUBROUTINE

  END MODULE

  PROGRAM  dtParamTypParamDef2
  USE M
  TYPE (DT(1,4))    :: T1
  TYPE (DT(2,4))    :: T2
  TYPE (DT(4,4))    :: T4
  TYPE (DT(8,4))    :: T8


  CALL T1%Proc()
  CALL T2%Proc()
  CALL T4%Proc()
  CALL T8%Proc()

  END

