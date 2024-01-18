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
!* (ICE)
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


  PROGRAM Arg8
  USE M
  IMPLICIT NONE
  TYPE(Base)  :: ExtFun
  PROCEDURE() :: ExtFun

  TYPE(Base) :: ExtFun1
  EXTERNAL   :: ExtFun1

  CALL Intsub(ExtFun )
  CALL Intsub(ExtFun1)


  CONTAINS

    SUBROUTINE IntSub(Arg)
    PROCEDURE(TYPE(Base))    :: Arg
    TYPE(Base)               :: V
    TYPE(Base), TARGET       :: Tar=Base("abc", NULL())
      V = Arg(Base("123", Tar))
      IF (V%C .NE. "123")                STOP 11
      IF (.NOT. ASSOCIATED(V%BPtr, Tar)) STOP 12
      IF (V%BPtr%C .NE. "abc" )          STOP 13
    END SUBROUTINE

  END

