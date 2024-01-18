!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_misc_2.f
!*  TEST CASE TITLE          :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : April 29 2011
!*  ORIGIN                     : Compiler Development IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Internal procedure as actual argument or procedure target
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : CMVC Feature number 303977
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : 
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*
!*  Miscellaneous Test  --  
!*    Recursinve internal function 
!*   
!*  (388880) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM intproc_misc_2
  PROCEDURE(INTEGER), POINTER :: procptr
  INTEGER :: mark

  DO i = 1, 100
    procptr => intfunc
    mark = 1
    IF ( procptr(intfunc, i) .NE. 1000) ERROR STOP 12
  END DO

  CONTAINS

    RECURSIVE FUNCTION  intfunc(Proc, iarg) 
    PROCEDURE(INTEGER) :: proc
    INTEGER, SAVE :: V
    INTEGER, TARGET :: iarg
 
    IF ( mark == 1) THEN
      V = 1
      mark = 2
      intfunc = Proc(Intfunc, V)
    ELSE
      V = V + 1
      IF ( V /= Iarg ) ERROR STOP 11
      IF ( V == 1000) THEN
        intfunc = 1000
      ELSE
        !intfunc = Proc(Intfunc, V)
        intfunc = intfunc(Intfunc, V)
      END IF
    END IF
    END FUNCTION 
  END


