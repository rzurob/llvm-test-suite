! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:  tcomp FuncRet8.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncRet8.f
!*
!*  DATE                       : May. 27, 2005
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
!*  Function Return - Interface on array bound
!*  (304351)(304465)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

  FUNCTION F(Arg)
  INTEGER:: Arg(:)
  INTEGER :: F(SIZE(Arg))
    F= Arg
  END FUNCTION

  FUNCTION F1(Arg)
  INTEGER:: Arg(:)
  INTEGER :: F1(1)
    F1 = Arg
  END FUNCTION

  END MODULE

  PROGRAM FuncRet8
  USE M
  PROCEDURE(F), POINTER :: P
  P=>F1
  END


