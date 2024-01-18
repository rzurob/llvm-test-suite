!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_misc_4.f
!*  TEST CASE TITLE          :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May 02, 2011
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
!*    Recursinve internal function returning data pointer 
!*   
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM intproc_misc_4
  PROCEDURE(intfunc), POINTER :: procptr
  INTEGER, TARGET :: mark

  DO i = 1, 100
    mark = 1
    procptr => intfunc
    IF ( .NOT. associated(procptr(i), Mark)) ERROR STOP 12
    mark = 1
    IF ( procptr(i) .NE. Mark) ERROR STOP 13
    procptr => null()
  END DO

  CONTAINS

    RECURSIVE FUNCTION  intfunc(iarg)
    INTEGER, POINTER :: intfunc
    INTEGER, SAVE :: V
    INTEGER, TARGET :: iarg

    IF ( mark == 1) THEN
      Mark  = 2
      V = 1
      intfunc => intfunc(V+1)
    ELSE
      V = V + 1
      IF ( V /= Iarg ) ERROR STOP 11
      IF ( V == 10) THEN
        intfunc => Mark
      ELSE
        intfunc => intfunc(V)
      END IF
    END IF
    END FUNCTION
  END

