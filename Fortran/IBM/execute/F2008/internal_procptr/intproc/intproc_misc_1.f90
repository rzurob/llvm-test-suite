!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_misc_1.f
!*
!*  DATE                       : April 29 2011
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
!*    Recursinve internal subroutine
!*
!*  (389283)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM intproc_misc_1
  INTEGER :: mark
  PROCEDURE(), POINTER :: procptr

  procptr => intsub

  DO i = 1, 100
    mark = 1
    CALL Intsub(intsub, 1)
    IF ( mark /= 1000) ERROR STOP 12
  END DO

  CONTAINS

    RECURSIVE SUBROUTINE intface(Proc, iarg)
    PROCEDURE(), POINTER :: proc
    END SUBROUTINE

    RECURSIVE SUBROUTINE intsub(Proc, iarg)
    !PROCEDURE(intface), POINTER :: proc
    PROCEDURE() :: proc
    INTEGER, SAVE :: V
    IF ( Iarg == 1000) THEN
      mark = Iarg
      RETURN
    ELSE
      IF ( mark == 1) THEN
        V = 1
        mark = 2
      ELSE
        V = V + 1
        IF ( V /= Iarg ) ERROR STOP 11
      END IF
      !CALL Proc(Intsub, V+1)
      CALL Proc(procptr, V+1)
    END IF
    END SUBROUTINE
  END


