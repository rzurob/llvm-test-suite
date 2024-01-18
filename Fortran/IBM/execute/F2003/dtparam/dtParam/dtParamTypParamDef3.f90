!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypParamDef3
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
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K, L)
    INTEGER(KIND=4), KIND :: K
    INTEGER(KIND=K), LEN  :: L=K
    CONTAINS
    GENERIC :: Proc => ModSub0
    PROCEDURE ::  ModSub0
  END TYPE

  TYPE, EXTENDS(DT0) :: DT1(K1, L1)
    INTEGER(KIND=K), KIND :: K1=K
    INTEGER(KIND=K), LEN  :: L1=K
    INTEGER(KIND=K)       :: KK=K
    INTEGER(KIND=K)       :: LL(L, L)=K
    CONTAINS
    GENERIC :: Proc => ModSub2, ModSub8
    PROCEDURE ::  ModSub2
    PROCEDURE ::  ModSub8
  END TYPE

  CONTAINS

  SUBROUTINE ModSub0(Arg)
  CLASS(DT0(1,*)) :: Arg
    IF ( Arg%K             .NE. 1 )         STOP 11
    IF ( Arg%L             .NE. 4)          STOP 12
  END SUBROUTINE

  SUBROUTINE ModSub2(Arg)
  CLASS(DT1(2,*, L1=*)) :: Arg
    IF ( Arg%K             .NE. Arg%KK )          STOP 21
    IF ( Arg%K             .NE. KIND(Arg%KK) )    STOP 22
    IF ( Arg%K1            .NE. 2 )               STOP 21
    IF ( KIND(Arg%LL)      .NE. Arg%K )           STOP 23
    IF ( ANY(SHAPE(Arg%LL) .NE. (/Arg%L, Arg%L/)))STOP 24
    IF ( ANY(Arg%LL        .NE. Arg%K) )          STOP 25
  END SUBROUTINE

  SUBROUTINE ModSub8(Arg)
  CLASS(DT1(8,*, L1=*)) :: Arg
    IF ( Arg%K             .NE. Arg%KK )          STOP 21
    IF ( Arg%K             .NE. KIND(Arg%KK) )    STOP 22
    IF ( Arg%K1            .NE. 8 )               STOP 21
    IF ( KIND(Arg%LL)      .NE. Arg%K )           STOP 23
    IF ( ANY(SHAPE(Arg%LL) .NE. (/Arg%L, Arg%L/)))STOP 24
    IF ( ANY(Arg%LL        .NE. Arg%K) )          STOP 25
  END SUBROUTINE


  END MODULE

  PROGRAM  dtParamTypParamDef3
  USE M
  TYPE (DT0(1,4))       :: T1
  TYPE (DT1(2,4,2,4))   :: T2
  TYPE (DT1(8,8,8,4))   :: T8


  CALL T1%Proc()
  CALL T2%Proc()
  CALL T8%Proc()

  END


