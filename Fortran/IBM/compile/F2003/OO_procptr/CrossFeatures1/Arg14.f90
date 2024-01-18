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
! %POSTCMD: tcomp Arg14.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Arg14.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 23, 2005
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
!*  Dummy procedure - Characteristics 
!*  Optional 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    INTERFACE
      FUNCTION ExtF(Arg)
        INTEGER :: Arg
        INTEGER:: ExtF
      END FUNCTION
    END INTERFACE

    INTERFACE
      FUNCTION ExtF1(Arg)
        INTEGER, OPTIONAL :: Arg
        INTEGER:: ExtF
      END FUNCTION
    END INTERFACE

  END MODULE
 
  PROGRAM Arg14
  USE M
  IMPLICIT NONE
  PROCEDURE(ExtF), POINTER :: ProcPtr=>NULL()
  PROCEDURE(ExtF1), POINTER :: ProcPtr1=>NULL()
 
  CALL IntSub(ProcPtr)
  CALL IntSub1(ProcPtr1)

  CONTAINS

  SUBROUTINE IntSub(Arg)
  IMPLICIT NONE
  PROCEDURE(ExtF1), POINTER :: Arg
  END SUBROUTINE

  SUBROUTINE IntSub1(Arg)
  IMPLICIT NONE
  PROCEDURE(ExtF), POINTER :: Arg
  END SUBROUTINE

  END

