! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 20, 2005
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
!*  Pointer assignment
!*
!* (304184)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      CHARACTER(3) :: C
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base), INTENT(IN) :: Arg
        TYPE(Base):: IntF(3)
      END FUNCTION
    END INTERFACE

  END MODULE


  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base), INTENT(IN) :: Arg
  TYPE(Base) :: ExtFun(3)
    ExtFun = Arg
  END FUNCTION


  PROGRAM Misc3
  USE M

  IMPLICIT TYPE(Base)(P)
  PROCEDURE(IntF) :: ExtFun

  PROCEDURE(IntF),       POINTER :: ProcPtr0
  PROCEDURE(TYPE(Base)), POINTER :: ProcPtr1
  PROCEDURE(),           POINTER :: ProcPtr2

  ProcPtr0 => ExtFun
  IF ( .NOT. ASSOCIATED(ProcPtr0, ExtFun) ) ERROR STOP 10

  ProcPtr1 => ExtFun
  IF ( .NOT. ASSOCIATED(ProcPtr1, ExtFun) ) ERROR STOP 11

  ProcPtr2 => ExtFun
  IF ( .NOT. ASSOCIATED(ProcPtr2, ExtFun) ) ERROR STOP 12

  END

