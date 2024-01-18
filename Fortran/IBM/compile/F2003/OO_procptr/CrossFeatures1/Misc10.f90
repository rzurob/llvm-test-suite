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
! %POSTCMD: tcomp Misc10.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  Misc10.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 08, 2005
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
!*  
!*  Procedure pointer can not be in IO 
!*  
!*  
!*  (304914)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc10 
  IMPLICIT INTEGER(P) 

  PROCEDURE(),        POINTER :: ProcPtr1 => NULL()
  PROCEDURE(INTEGER), POINTER :: ProcPtr2 => NULL()


  READ *, ProcPtr1 
  READ *, ProcPtr2 
  
  PRINT *,ProcPtr1
  PRINT *,ProcPtr2
  

  END


