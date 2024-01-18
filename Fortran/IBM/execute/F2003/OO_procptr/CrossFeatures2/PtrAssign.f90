! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 13, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
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
!*  Procedure pointer
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  PROCEDURE (IntF), POINTER :: ProcPtr => NULL()

  INTERFACE
    FUNCTION IntF(Arg)
    CLASS(*), ALLOCATABLE  :: IntF
    CLASS(*)               :: Arg
    END FUNCTION
  END INTERFACE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(*), ALLOCATABLE  :: ModFun
  CLASS(*)               :: Arg
    ALLOCATE(ModFun, SOURCE=Arg)
  END FUNCTION

  END MODULE

  PROGRAM  PtrAssign
  USE M, LProcPtr => ProcPtr, LModFun => ModFun, LIntF => IntF
  IMPLICIT NONE
  PROCEDURE (LIntF), POINTER :: LProcPtr1

    LProcPtr => LModFun
    CALL IntSub( LProcPtr(10_1), 10_1 )

    LProcPtr1 => LModFun
    CALL IntSub( LProcPtr(1_1), 1_1 )

    CALL IntSub(LProcPtr(LProcPtr(1_1)), 1_1 )
    CALL IntSub(LProcPtr1(LProcPtr1(2_1)), 2_1 )

    CALL IntSub( LProcPtr(LProcPtr1(127_1)), 127_1 )
    CALL IntSub( LProcPtr(-128_1),  -128_1 )

  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2)
  CLASS(*)    :: Arg1
  INTEGER(1)  :: Arg2

  SELECT TYPE( Arg1 )
  TYPE IS(INTEGER(1))
    IF (Arg1 .NE. Arg2) STOP 11
  CLASS DEFAULT
    STOP 12
  END SELECT

  END SUBROUTINE

  END

