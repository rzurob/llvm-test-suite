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
!*  TEST CASE NAME             : FuncRet8.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 27, 2005
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
!*  (304354)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  CONTAINS

  FUNCTION F(Arg)
  INTEGER :: Arg(:)
  INTEGER :: F(SIZE(Arg))
    F= Arg
  END FUNCTION
 
  END MODULE

  FUNCTION ExtFun(Arg)
  INTEGER :: Arg(:)
  INTEGER :: ExtFun(SIZE(Arg))
    ExtFun = Arg
  END FUNCTION
 
  PROGRAM Common 
  USE M
  PROCEDURE(F), POINTER :: ProcPtr
  PROCEDURE(F)          :: ExtFun 
  COMMON ProcPtr

  ProcPtr => ExtFun
  CALL IntSub()
  IF ( ASSOCIATED(ProcPtr) ) STOP 12
   
   
  CONTAINS

  SUBROUTINE IntSub()
  PROCEDURE(F), POINTER :: ProcPtr
  COMMON ProcPtr
  INTEGER :: V(1024)
  INTEGER :: I

  V = ProcPtr((/(I, I=1,1024)/))
  IF ( ANY(V .NE. (/(I, I=1,1024)/)) ) STOP 11
 
  NULLIFY(ProcPtr)
 
  END SUBROUTINE 


  END


