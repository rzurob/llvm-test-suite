! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 9, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  The allocate stmt
!*
!*  (306345)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE
      FUNCTION IntF(Arg)
        PROCEDURE(INTEGER), POINTER :: IntF
        PROCEDURE(INTEGER)          :: Arg
      END FUNCTION
    END INTERFACE

    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(IntF),NOPASS,  POINTER :: ProcPtr1=>NULL()
      PROCEDURE(INTEGER),NOPASS,  POINTER :: ProcPtr2=>NULL()
    END TYPE
    CONTAINS

    FUNCTION ProcFun(Arg)
    PROCEDURE(INTEGER), POINTER :: ProcFun
    PROCEDURE(INTEGER)          :: Arg
      ProcFun => Arg
    END FUNCTION

    FUNCTION Fun(Arg)
    INTEGER  :: Fun
    INTEGER  :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Allocate3
  USE M
  IMPLICIT NONE
  TYPE(DT)              :: V
  TYPE(DT), POINTER     :: VP
  TYPE(DT), ALLOCATABLE :: VA

  ALLOCATE(VP)
  IF ( .NOT. ASSOCIATED( VP ) )        ERROR STOP 11
  IF (       ASSOCIATED( VP%ProcPtr2)) ERROR STOP 12

  VP%ProcPtr2 => Fun
  IF ( .NOT. ASSOCIATED( VP%ProcPtr2 ))    ERROR STOP 13
  V = DT(VP%ProcPtr2(-1), NULL(), VP%ProcPtr2 )
  IF ( V%Id .NE. -1 )                      ERROR STOP 14
  IF ( .NOT. ASSOCIATED(V%ProcPtr2, Fun) ) ERROR STOP 15

  NULLIFY(VP%ProcPtr2)
  DEALLOCATE(VP)

  ALLOCATE(VA)
  IF ( .NOT. ALLOCATED( VA ))           ERROR STOP 21
  IF (       ASSOCIATED( VA%ProcPtr1 )) ERROR STOP 22

  VA%ProcPtr1 => ProcFun
  IF ( .NOT. ASSOCIATED( VA%ProcPtr1, ProcFun ))  ERROR STOP 23
  VA = DT(-2, VA%ProcPtr1, NULL())
  IF ( VA%Id .NE. -2 ) ERROR STOP 24
  IF ( .NOT. ASSOCIATED( VA%ProcPtr1, ProcFun ))  ERROR STOP 25

  NULLIFY(VA%ProcPtr2)
  DEALLOCATE(VA)

  END


