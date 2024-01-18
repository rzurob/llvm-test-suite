! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 23, 2005
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
!*  declared to have the EXTERNAL attribute-
!*  External
!*
!* (Comp failed)
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
        TYPE(Base):: IntF
      END FUNCTION
    END INTERFACE

  END MODULE

  MODULE M
  USE M0

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(Base) :: Arg
  TYPE(Base) :: ExtFun
    ExtFun = Arg
  END FUNCTION

  FUNCTION ExtFun1(Arg)
  USE M
  TYPE(Base) :: Arg
  TYPE(Base) :: ExtFun1
    ExtFun1 = Arg
  END FUNCTION


  PROGRAM Arg9
  USE M
  IMPLICIT NONE

  PROCEDURE() :: ExtFun
  TYPE(Base)  :: ExtFun

  EXTERNAL   :: ExtFun1
  TYPE(Base) :: ExtFun1

  CALL Intsub(ExtFun )
  CALL Intsub(ExtFun1)

  CALL IntSub1(extFun)
  CALL IntSub1(extFun1)

  CALL IntSub2(extFun)
  CALL IntSub2(extFun1)

  CONTAINS

    SUBROUTINE IntSub(Arg)
    IMPLICIT TYPE(Base)(A)
    PROCEDURE()        :: Arg
    TYPE(Base)         :: V
    TYPE(Base), TARGET :: Tar=Base("abc", NULL())
      V = Arg(Base("123", Tar))
      IF (V%C .NE. "123")                ERROR STOP 11
      IF (.NOT. ASSOCIATED(V%BPtr, Tar)) ERROR STOP 12
      IF (V%BPtr%C .NE. "abc" )          ERROR STOP 13
    END SUBROUTINE

    SUBROUTINE IntSub1(Arg)
    IMPLICIT TYPE(Base)(A)
    ! PROCEDURE() :: Arg  ! This is causing trouble
    PROCEDURE(TYPE(Base)) :: Arg
      CALL IntSub(Arg)
    END SUBROUTINE

    SUBROUTINE IntSub2(Arg)
    TYPE(Base)  :: Arg
    PROCEDURE() :: Arg
      CALL IntSub(Arg)
    END SUBROUTINE

  END

