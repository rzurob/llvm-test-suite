! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Misc5.f 
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc5.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 31, 2005
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
!*  Wrong diagnosis: when interface is specified for proc ptr 
!*  The err msg:
!*  The procedure pointer assignment statement target #1 is invalid.  
!*   The target must be: a module procedure, an intrinsic procedure, 
!*   a dummy procedure that is not a procedure pointer, or a function reference 
!*   that returns a procedure pointer or another procedure pointer. 
!* (304446)
!*
!234567890123456789012345678901234567890123456789012345678901234567890
 

  PROGRAM Misc5 

  INTERFACE
    FUNCTION IFun(Arg)
    INTEGER, ALLOCATABLE :: IFun
    INTEGER :: Arg
    END FUNCTION
  END INTERFACE

  PROCEDURE(IFUN), POINTER :: ProcPtr
  ProcPtr => NULL(ProcPtr)

  END


