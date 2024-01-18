!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_misc_15.f
!*
!*  DATE                       : May 04, 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Miscellaneous Test  --
!*    internal function returning procedure pointer as actual argument
!*    The corresponding summy is pointer
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  intproc_misc_15
  INTEGER Arr(1000)

  DO I = 1, 1000
    !CALL sub(intfunc, set, I)
    CALL sub(set, I)
    IF (ANY(arr .NE. I)) ERROR STOP 11
  END DO

  CONTAINS

    SUBROUTINE set(iarg)
      arr = iarg
    END SUBROUTINE

    FUNCTION intfunc(proc)
    PROCEDURE() :: proc
    PROCEDURE(), POINTER :: intfunc
      intfunc => proc
    END FUNCTION

    SUBROUTINE sub(proc, Iarg)
    PROCEDURE(), POINTER, INTENT(IN) :: proc
     !PROCEDURE(), pointer :: procptr
     !procptr => proc1(proc2)
     !CALL procptr(Iarg)
      CALL proc(Iarg)
    END SUBROUTINE

  END

