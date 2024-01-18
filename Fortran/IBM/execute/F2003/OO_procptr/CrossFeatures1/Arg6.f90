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
!*  If an external procedure name or a dummy procedure name is used as an actual
!*  argument, its interface shall be explicit or it shall be explicitly
!*  declared to have the EXTERNAL attribute
!*
!* (304048)(305976)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0

    TYPE :: Base
      CHARACTER(3) :: C
      TYPE(Base), POINTER :: BPtr
    END TYPE

    INTERFACE
      FUNCTION IntF(Arg)
      IMPORT
        TYPE(Base) :: Arg
        TYPE(Base), ALLOCATABLE :: IntF
      END FUNCTION
    END INTERFACE

  END MODULE

  MODULE M
  USE M0

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base) :: Arg
  TYPE(Base), ALLOCATABLE :: ExtFun
    !ALLOCATE(ExtFun, SOURCE=Arg) ! not 10.1
    ALLOCATE(ExtFun)
    ExtFun = Arg
  END FUNCTION

  PROGRAM Arg6
  USE M
  IMPLICIT NONE
  PROCEDURE(IntF) :: ExtFun

  CALL IntSub1(ExtFun )


  CONTAINS

  SUBROUTINE IntSub1(Arg)
  PROCEDURE(IntF) :: Arg
  TYPE(Base) :: V
  TYPE(Base), TARGET :: Tar=Base("abc", NULL())
    V = Arg(Base("123", Tar))
    IF (V%C .NE. "123")                ERROR STOP 11
    IF (.NOT. ASSOCIATED(V%BPtr, Tar)) ERROR STOP 12
    IF (V%BPtr%C .NE. "abc" )          ERROR STOP 13
  END SUBROUTINE

  END

