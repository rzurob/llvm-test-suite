! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignCharacteristics.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignCharacteristics.f
!*
!*  DATE                       : Mar. 18, 2005
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
!*  proc-pointer-object is not pure while proc-target may be pure
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PURE FUNCTION ExtFun(Arg)
  INTEGER(8), POINTER    :: ExtFun
  INTEGER(8), INTENT(IN) :: Arg
    !ALLOCATE(ExtFun, SOURCE=Arg) ! not 10.1
    ALLOCATE(ExtFun)
    ExtFun = Arg
  END FUNCTION

  PROGRAM PtrAssignCharacteristics
  IMPLICIT NONE

  INTERFACE
    FUNCTION ExtF1(Arg)
      INTEGER(8), POINTER    :: ExtF1
      INTEGER(8), INTENT(IN) :: Arg
    END FUNCTION
  END INTERFACE

  INTERFACE
    PURE FUNCTION ExtF2(Arg)
      INTEGER(8), POINTER    :: ExtF2
      INTEGER(8), INTENT(IN) :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(ExtF2)          :: ExtFun
  PROCEDURE(ExtF1), POINTER :: ProcPtr

  INTEGER :: i

  ProcPtr => ExtFun

  IF ( .NOT. ASSOCIATED(ProcPtr) )          STOP 21
  IF ( .NOT. ASSOCIATED(ProcPtr, ExtFun) )  STOP 22
  IF ( .NOT. ASSOCIATED(ProcPtr, ProcPtr))  STOP 23

  IF (  ProcPtr(12345678_8) .NE. 12345678_8 )    STOP 31

  END

