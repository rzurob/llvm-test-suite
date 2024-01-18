! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Arg7.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg7.f
!*
!*  DATE                       : Jun. 27, 2005
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
!*  Bind(c)
!*  (314816/316676)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    USE ISO_C_BINDING

    TYPE, BIND(C) :: DT
      CHARACTER(C_CHAR) :: C
    END TYPE

    CONTAINS

    FUNCTION ModFun(Arg) BIND(C)
    TYPE(DT), INTENT(IN) :: Arg
    TYPE(DT)             :: ModFun
      ModFun=Arg
    END FUNCTION

    FUNCTION IFun(Arg) BIND(C)
    TYPE(DT), INTENT(IN) :: Arg
    TYPE(DT)             :: IFun
      IFun=Arg
    END FUNCTION

  END MODULE

  PROGRAM Arg7
  USE M
  IMPLICIT NONE
  PROCEDURE(IFun), POINTER :: ProcPtr

  CALL IntSub1(ModFun )

  ProcPtr => ModFun
  CALL IntSub1( ProcPtr )

  CALL IntSub2( ProcPtr )

  CONTAINS

  SUBROUTINE IntSub1(Arg)
  PROCEDURE(IFun) :: Arg
  TYPE(DT)  :: V

    V = Arg(DT("1"))
    IF (V%C .NE. "1")                     STOP 11

  END SUBROUTINE

  SUBROUTINE IntSub2(Arg)
  PROCEDURE(IFun), POINTER :: Arg
  TYPE(DT) :: V

    V = Arg(DT("2"))
    IF (V%C .NE. "2")                     STOP 31

  END SUBROUTINE

  END

