! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignImp4.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignImp4.f
!*
!*  DATE                       : Mar. 27, 2005
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
!*  If proc-target and proc-pointer-object are functions,
!*  they shall have the same type; corresponding type parameters
!*  shall either both be deferred or both have the same value.
!*
!*  (304498)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  FUNCTION ExtFun1(Arg)
  CHARACTER(*) :: Arg
  CHARACTER    :: ExtFun1
    ExtFun1 = Arg(1:1)
  END FUNCTION

  FUNCTION ExtFun2(Arg)
  CHARACTER(*) :: Arg
  CHARACTER(LEN(Arg)) :: ExtFun2
    ExtFun2 = Arg
  END FUNCTION

  PROGRAM PtrAssignImp4
  IMPLICIT NONE

  INTERFACE
    FUNCTION ExtFun2(Arg)
      CHARACTER(*) :: Arg
      CHARACTER(LEN(Arg)) :: ExtFun2
    END FUNCTION
  END INTERFACE

  PROCEDURE(CHARACTER)           :: ExtFun1
  PROCEDURE(CHARACTER),  POINTER :: ProcPtr1

  PROCEDURE(ExtFun2),  POINTER :: ProcPtr2

  ProcPtr1 => ExtFun1
  IF ( ProcPtr1("1234567") .NE. ExtFun1("1") ) STOP 11

  ProcPtr2 => ExtFun2
  IF ( ProcPtr2("1234567890") .NE. "1234567890" ) STOP 12

  END

