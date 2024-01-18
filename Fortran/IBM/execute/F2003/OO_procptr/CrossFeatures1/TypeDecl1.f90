! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 07, 2005
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
!*  Function-names declared shall be the name of an external function
!*  or function dummy procedure
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: Base
    CHARACTER(3) :: C
  END TYPE

  TYPE :: Base1
    CHARACTER(3) :: C
    PROCEDURE(), POINTER, NOPASS :: ProcPtr
  END TYPE

  END MODULE

  FUNCTION ExtFun()
  USE M
  TYPE(Base) :: ExtFun
    ExtFun = Base("123")
  END FUNCTION

  FUNCTION ExtFun1()
  USE M
  TYPE(Base1)  :: ExtFun1
  PROCEDURE()  :: ExtSub
    ExtFun1 = Base1("123", ExtSub)
  END FUNCTION

  SUBROUTINE ExtSub()
  END SUBROUTINE

  PROGRAM TypeDecl1
  USE M
  IMPLICIT TYPE(Base)(P)

  PROCEDURE(TYPE(Base)), POINTER :: ProcPtr => NULL()

  PROCEDURE() :: ExtSub

  PROCEDURE() :: ExtFun
  TYPE(Base)  :: ExtFun
  TYPE(Base)  :: V

  TYPE(Base1) :: ExtFun1
  TYPE(Base1) :: U
  PROCEDURE() :: ExtFun1

  V = ExtFun()
  IF ( V%C .NE. "123" ) STOP 11

  U = ExtFun1()
  IF ( U%C .NE. "123" ) STOP 12
  IF ( .NOT. ASSOCIATED(U%ProcPtr, ExtSub) ) STOP 13

  END


