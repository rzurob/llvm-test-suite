! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp Arg19.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg19.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 25, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 289058 
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
!*  If the interface of the dummy argument is implicit and either the name of 
!*  the dummy argument is  explicitly typed or it is referenced as a function, 
!*  the dummy argument shall not be referenced as a subroutine and the actual
!*  argument shall be a function, function procedure pointer, or dummy procedure.
!*
!* ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  CONTAINS
  FUNCTION F()
    F=1.0
  END FUNCTION
  END MODULE

  PROGRAM Arg19
  USE M
  PROCEDURE(), POINTER :: ProcPtr
  PROCEDURE(REAL), POINTER :: ProcPtr1
  PROCEDURE() :: ExtSub 

  ProcPtr => F

  IF ( .TRUE. ) THEN
    CALL ProcPtr()
  ELSE
    PRINT *, ProcPtr()
  END IF

  ProcPtr1 => F
  CALL ProcPtr1()

  CALL IntSub1(IntSub)
  CALL IntSub1(ExtSub)

  CONTAINS

  SUBROUTINE IntSub(Proc)
  PROCEDURE(REAL), POINTER :: ProcPtr
    CALL ProcPtr()
  END SUBROUTINE

  SUBROUTINE IntSub1(Proc)
  PROCEDURE(REAL) :: ProcPtr
  END SUBROUTINE

  END
  
  SUBROUTINE ExtSub()
  END SUBROUTINE

