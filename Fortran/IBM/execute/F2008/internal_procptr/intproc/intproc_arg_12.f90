!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_arg_12.f
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
!*  Test the argument association --
!*    The dummy is procedure pointer with intent & optional attributes 
!*  
!*  
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M 

  CONTAINS
  SUBROUTINE Modsub()
    INTEGER, SAVE :: iii
 
    iii = -1 
    DO I=1, 100 
      CALL Check(-1)
      CALL intsub(intset, i)
      CALL Check(-1)
    END DO
    
    CONTAINS

    SUBROUTINE intsub(Proc, Arg)
    PROCEDURE(intset), POINTER, INTENT(IN), OPTIONAL :: Proc
    INTEGER :: Arg
      IF ( PRESENT(proc) ) THEN 
        CALL Proc(Arg)
      ELSE
        ERROR STOP 12
      END IF
    END SUBROUTINE

    SUBROUTINE intset(Arg)
    INTEGER :: Arg
    INTEGER, SAVE :: iii !no conflict with iii in modsub 
      iii = Arg 
    END SUBROUTINE

    SUBROUTINE Check(Arg)
    INTEGER Arg
      IF ( iii .NE. Arg) ERROR STOP 11 
    END SUBROUTINE

  END SUBROUTINE
  END MODULE
 
  PROGRAM intproc_arg_12
  USE M
    CALL Modsub()
  END



