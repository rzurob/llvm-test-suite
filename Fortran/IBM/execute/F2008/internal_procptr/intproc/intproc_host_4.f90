!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_host_4.f
!*  TEST CASE TITLE          :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : April 27 2011
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*
!*  Test host instance --
!*    The host instance of an internal procedure that is
!*    invoked via a dummy procedure is the host instance 
!*    of the associating entity from when the argument association 
!*    was established 
!*    
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890


 
  PROGRAM intproc_host_4
  INTEGER :: iarr(1000)

  DO i = 1, 100
    iarr = i
    CALL intsub(intsub1, i)
    IF ( ANY(iarr .NE. i+4) ) ERROR STOP 15
  END DO

  CONTAINS
    SUBROUTINE intsub(proc, iarg) 
    PROCEDURE() :: proc 
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 11
    iarr = iarg + 1 
    CALL proc(intsub2, iarg+1)
    END SUBROUTINE

    SUBROUTINE intsub1(proc, iarg) 
    PROCEDURE() :: proc 
    IF ( ANY(iarr .NE. iarg) ) ERROR STOP 12
    iarr = iarg+1 
    CALL proc(intsub3, iarg+1)
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


