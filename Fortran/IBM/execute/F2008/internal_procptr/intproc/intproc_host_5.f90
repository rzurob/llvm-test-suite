!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_host_5.f
!*
!*  DATE                       : April 28 2011
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  Test host instance --
!*    The host instance of an internal procedure that is
!*    invoked via a procedure pointer association is the host instance
!*    of the associating entity from when the pointer association
!*    was established
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM intproc_host_5
  INTEGER :: iarr(1000)
  PROCEDURE(), POINTER :: procptr

  DO i = 1, 100
    iarr = i
    procptr => intsub1
    CALL intsub(procptr, i)
    IF ( ANY(iarr .NE. i+4) ) ERROR STOP 15
  END DO

  CONTAINS
    SUBROUTINE intsub(proc, iarg)
    PROCEDURE() :: proc
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 11
    iarr = iarg + 1
    procptr => intsub2
    CALL proc(procptr, iarg+1)
    END SUBROUTINE

    SUBROUTINE intsub1(proc, iarg)
    PROCEDURE() :: proc
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 12
    iarr = iarg+1
    procptr => intsub3
    CALL proc(procptr, iarg+1)
    END SUBROUTINE

    SUBROUTINE intsub2(proc, iarg)
    PROCEDURE() :: proc
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 13
    iarr = iarg + 1
    CALL proc(iarg+1)
    END SUBROUTINE

    SUBROUTINE intsub3(iarg)
    PROCEDURE() :: proc
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 14
    iarr = iarg + 1
    END SUBROUTINE

  END


