! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Null.f 
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
!*  TEST CASE NAME             : Null.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 10, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment 
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
!*   null()
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(Fun), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM NullTest 
  USE M
  IMPLICIT NONE 

  TYPE (DT) :: V
  PROCEDURE(Fun), POINTER :: ProcPtr=>NULL()

  ProcPtr => Fun 
  ProcPtr => NULL(V%ProcPtr) 
  IF ( ASSOCIATED(ProcPtr) ) STOP 11

  ProcPtr => Fun 
  ProcPtr => NULL(ProcPtr) 
  IF ( ASSOCIATED(ProcPtr) ) STOP 12

  V%ProcPtr => Fun 
  V%ProcPtr => NULL(V%ProcPtr) 
  IF ( ASSOCIATED(V%ProcPtr) ) STOP 13

  ProcPtr => NULL() 
  V%ProcPtr => Fun 
  V%ProcPtr => NULL(ProcPtr) 
  IF ( ASSOCIATED(V%ProcPtr) ) STOP 14

  ProcPtr => Fun 
  V%ProcPtr => Fun 
  V%ProcPtr => NULL(ProcPtr) 
  IF ( ASSOCIATED(V%ProcPtr) ) STOP 15

  END


