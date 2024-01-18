!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME           : intproc_host_2.f
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
!*    Access the variable with the SAVE attribute
!*  
!*  
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  SUBROUTINE Extsub()
 
  INTERFACE  
    SUBROUTINE Extcall(Proc, Arg)
    PROCEDURE() :: Proc
    INTEGER :: Arg
    END SUBROUTINE
  END INTERFACE


    DO I=1, 100 
      CALL Extcall(check, I)
    END DO
    
    CONTAINS

    SUBROUTINE Check(Arg)
    INTEGER, SAVE :: iii
    INTEGER :: Arg
      IF ( Arg == 1) THEN
        iii = 1 
      ELSE
        iii = iii + 1 
        IF ( iii .NE. Arg) ERROR STOP 11 
      END IF
    END SUBROUTINE


  END SUBROUTINE
 
  SUBROUTINE Extcall(Proc, Arg)
  PROCEDURE() :: Proc
  INTEGER :: Arg
     CALL Proc(Arg)
  END SUBROUTINE
 
  PROGRAM intproc_host_2
  EXTERNAL Extsub
    CALL Extsub()
  END

