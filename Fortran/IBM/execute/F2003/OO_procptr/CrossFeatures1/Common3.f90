! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Common3.f 
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
!*  TEST CASE NAME             : Common3.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 29, 2005
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
!*  Common block - blank/named 
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  INTERFACE
    FUNCTION Fun()
      INTEGER :: Fun
    END FUNCTION
  END INTERFACE 

  PROCEDURE(Fun), POINTER :: ProcPtr1
  PROCEDURE(Fun), POINTER :: ProcPtr2

  COMMON ProcPtr1 
  COMMON /blk/ ProcPtr2 

  INTERFACE
    FUNCTION Fun1()
      IMPORT Fun
      PROCEDURE(Fun), POINTER :: Fun1
    END FUNCTION
  END INTERFACE

  CONTAINS

  FUNCTION ModFun()
  INTEGER :: ModFun
    ModFun = -1
  END FUNCTION
 
  END MODULE

  FUNCTION ExtFun()
  INTEGER :: ExtFun
    ExtFun = -2
  END FUNCTION
 
  FUNCTION ExtFun1()
  USE M, ONLY: Fun 
  PROCEDURE(Fun), POINTER :: ProcPtr
  PROCEDURE(Fun), POINTER :: ExtFun1 
  COMMON ProcPtr
    ExtFun1 => ProcPtr 
  END FUNCTION
 
  FUNCTION ExtFun2()
  USE M, ONLY: Fun 
  PROCEDURE(Fun), POINTER :: ProcPtr
  PROCEDURE(Fun), POINTER :: ExtFun2 
  COMMON /blk/ ProcPtr
    ExtFun2 => ProcPtr 
  END FUNCTION
 

  PROGRAM Common3 
  USE M

  PROCEDURE(Fun)          :: ExtFun
  PROCEDURE(Fun1)         :: ExtFun1, ExtFun2
  PROCEDURE(Fun), POINTER :: ProcPtr

  ProcPtr1 => ModFun
  ProcPtr  => ExtFun1()
  IF ( .NOT. ASSOCIATED( ProcPtr, ModFun ) ) STOP 11

  ProcPtr2 => ExtFun
  ProcPtr  => ExtFun2()
  IF ( .NOT. ASSOCIATED( ProcPtr, ExtFun ) ) STOP 12

  CALL IntSub()
   
  CONTAINS

  SUBROUTINE IntSub()

  ProcPtr1 => ExtFun
  ProcPtr  => ExtFun1()
  IF ( .NOT. ASSOCIATED( ProcPtr, ExtFun ) ) STOP 21
   
  ProcPtr2 => ModFun
  ProcPtr  => ExtFun2()
  IF ( .NOT. ASSOCIATED( ProcPtr, ModFun ) ) STOP 22
   
  END SUBROUTINE

  END

