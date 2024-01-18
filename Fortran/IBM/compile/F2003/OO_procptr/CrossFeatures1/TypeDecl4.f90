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
! %POSTCMD: tcomp TypeDecl4.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             :  TypeDecl4.f
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 07, 2005
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
!*  If the VALUE attribute is specified, the EXTERNAL 
!*  shall not be specified. 
!*  
!*  (ICE-304817)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM TypeDecl4 

! Stop Compilation ! 

  CONTAINS

  SUBROUTINE IntSub(Proc, ProcPtr)

  VALUE        :: Proc 
  PROCEDURE()  :: Proc 

  VALUE                 :: ProcPtr
  PROCEDURE(), POINTER  :: ProcPtr 

  END SUBROUTINE

  END


