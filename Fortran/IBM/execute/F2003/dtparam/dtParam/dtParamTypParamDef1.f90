!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypParamDef1
!*
!*  DATE                       : Dec. 15, 2005
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

  TYPE :: DT1(K, L)
    INTEGER(KIND=1), KIND :: K=0
    INTEGER(KIND=1), LEN  :: L=0
    INTEGER(KIND=K)       :: KK=K
    INTEGER(KIND=K)       :: LL(L, L)=1
    CONTAINS
    PROCEDURE :: Proc => ModSub1
  END TYPE

  TYPE :: DT2(K, L)
    INTEGER(KIND=2), KIND :: K=0
    INTEGER(KIND=2), LEN  :: L=0
    INTEGER(KIND=K)       :: KK=K
    INTEGER(KIND=K)       :: LL(L, L)=1
    CONTAINS
    PROCEDURE :: Proc => ModSub2
  END TYPE

  TYPE :: DT4(K, L)
    INTEGER(KIND=4), KIND :: K=0
    INTEGER(KIND=4), LEN  :: L=0
    INTEGER(KIND=K)       :: KK=K
    INTEGER(KIND=K)       :: LL(L, L)=1
    CONTAINS
    PROCEDURE :: Proc => ModSub4
  END TYPE

  TYPE :: DT8(K, L)
    INTEGER(KIND=8), KIND :: K=0
    INTEGER(KIND=8), LEN  :: L=0
    INTEGER(KIND=K)       :: KK=K
    INTEGER(KIND=K)       :: LL(L, L)=1
    CONTAINS
    PROCEDURE :: Proc => ModSub8
  END TYPE



  CONTAINS

  SUBROUTINE ModSub1(Arg)
  CLASS(DT1(1,*)) :: Arg
    IF ( Arg%K             .NE. Arg%KK )          STOP 11
    IF ( Arg%K             .NE. KIND(Arg%KK) )    STOP 12
    IF ( ANY(SHAPE(Arg%LL) .NE. (/Arg%L, Arg%L/)))STOP 13
    IF ( ANY(Arg%LL        .NE. 1) )              STOP 14
  END SUBROUTINE

  SUBROUTINE ModSub2(Arg)
  CLASS(DT2(2,*)) :: Arg
    IF ( Arg%K             .NE. Arg%KK )          STOP 21
    IF ( Arg%K             .NE. KIND(Arg%KK) )    STOP 22
    IF ( ANY(SHAPE(Arg%LL) .NE. (/Arg%L, Arg%L/)))STOP 23
    IF ( ANY(Arg%LL        .NE. 1) )              STOP 24
  END SUBROUTINE

  SUBROUTINE ModSub4(Arg)
  CLASS(DT4(4,*)) :: Arg
    IF ( Arg%K             .NE. Arg%KK )          STOP 41
    IF ( Arg%K             .NE. KIND(Arg%KK) )    STOP 42
    IF ( ANY(SHAPE(Arg%LL) .NE. (/Arg%L, Arg%L/)))STOP 43
    IF ( ANY(Arg%LL        .NE. 1) )              STOP 44
  END SUBROUTINE

  SUBROUTINE ModSub8(Arg)
  CLASS(DT8(8,*)) :: Arg
    IF ( Arg%K             .NE. Arg%KK )          STOP 81
    IF ( Arg%K             .NE. KIND(Arg%KK) )    STOP 82
    IF ( ANY(SHAPE(Arg%LL) .NE. (/Arg%L, Arg%L/)))STOP 83
    IF ( ANY(Arg%LL        .NE. 1) )              STOP 84
  END SUBROUTINE

  END MODULE

  PROGRAM  dtParamTypParamDef1
  USE M
  TYPE (DT1(1,4))    :: T1
  TYPE (DT2(2,8))    :: T2
  TYPE (DT4(4,64))   :: T4
  TYPE (DT8(8,1024)) :: T8


  CALL T1%Proc()

  CALL T2%Proc()

  CALL T4%Proc()

  CALL T8%Proc()

  END


