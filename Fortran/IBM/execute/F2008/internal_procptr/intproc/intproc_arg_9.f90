!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_arg_9.f
!*  TEST CASE TITLE          :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : April 25 2011
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
!*  Test the argument association --
!*    The dummy is procedure pointer and
!*    The actual argument is a procedure pointer associated with an internal procedure. 
!*    
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M 

  CONTAINS
  SUBROUTINE Modsub()
    INTEGER, SAVE :: iii
    PROCEDURE(), POINTER :: procptr
 
    iii = 0 
    DO I=1, 100 
      CALL Check(i-1)
      procptr => intset
      CALL intsub(procptr, i)
      CALL Check(i)
    END DO
    
    CONTAINS

    SUBROUTINE intsub(Proc, Arg)
    PROCEDURE(intset), POINTER :: Proc
    INTEGER :: Arg 
      CALL Proc(Arg)
    END SUBROUTINE

    SUBROUTINE intset(Arg)
    INTEGER :: Arg 
      iii = Arg 
    END SUBROUTINE

    SUBROUTINE Check(Arg)
    INTEGER Arg
      IF ( iii .NE. Arg) ERROR STOP 11 
    END SUBROUTINE

  END SUBROUTINE
  END MODULE
 
  PROGRAM intproc_arg_9
  USE M
    CALL Modsub()
  END



