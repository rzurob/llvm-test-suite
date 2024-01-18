! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Common.f 
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
!*  TEST CASE NAME             : Common.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 27, 2005
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
!*  Common block 
!*  (314890)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

  FUNCTION F(Arg)
  CLASS(*), TARGET :: Arg(:)
  CLASS(*), POINTER :: F(:)
    F => Arg
  END FUNCTION
 
  END MODULE

  FUNCTION ExtFun(Arg)
  CLASS(*), TARGET :: Arg(:)
  CLASS(*), POINTER :: ExtFun(:)
    ExtFun => Arg
  END FUNCTION
 
  PROGRAM Common0 
  USE M
  PROCEDURE(F), POINTER :: ProcPtr
  PROCEDURE(F)          :: ExtFun 
  COMMON ProcPtr

  ProcPtr => ExtFun
  CALL IntSub()
  IF ( ASSOCIATED(ProcPtr) ) STOP 13
   
   
  CONTAINS

  SUBROUTINE IntSub()
  PROCEDURE(F), POINTER :: ProcPtr
  COMMON ProcPtr
  INTEGER, TARGET :: V(1025)=(/(I, I=1,1025)/)
  INTEGER :: I

  SELECT TYPE ( As => ProcPtr(V))
  TYPE IS (INTEGER)
    IF ( ANY(As .NE. (/(I, I=1,1025)/)) ) STOP 11
  CLASS DEFAULT
    STOP 12
  END SELECT
 
  NULLIFY(ProcPtr)
 
  END SUBROUTINE 


  END


