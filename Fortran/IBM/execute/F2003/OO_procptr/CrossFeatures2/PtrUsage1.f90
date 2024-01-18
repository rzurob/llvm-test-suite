! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: PtrUsage1.f 
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
!*  TEST CASE NAME             : PtrUsage1.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 20, 2005
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
!*  Usage: Recursive 
!* 
!*  (315457) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M
  
  LOGICAL :: L = .FALSE.

  TYPE :: DT
    INTEGER                        :: ID=0
    PROCEDURE(ModSub), POINTER, PASS :: ProcPtr1=>NULL()
    PROCEDURE(ModFun), POINTER, PASS :: ProcPtr2=>NULL()
  END TYPE

  CONTAINS

  RECURSIVE SUBROUTINE ModSub(Arg)
  CLASS(DT) :: Arg

    IF (Arg%ID .NE. -1) STOP 11
    IF ( .NOT. L ) THEN
      L = .TRUE.
      Arg%ProcPtr1 => ModSub
      CALL Arg%ProcPtr1()
    END IF

  END SUBROUTINE

  RECURSIVE FUNCTION ModFun(Arg)
  CLASS(DT) :: Arg
  REAL      :: ModFun

    IF (Arg%ID .NE. -2) STOP 12
    IF ( .NOT. L ) THEN
      L = .TRUE.
      Arg%ProcPtr2 => ModFun
      ModFun = Arg%ProcPtr2()
    END IF

    ModFun = -1.0
 
  END FUNCTION

  END MODULE

  
  PROGRAM PtrUsage1 
  USE M
  IMPLICIT NONE 

  L = .FALSE.
  CALL ModSub(DT(-1, NULL()))
  IF ( .NOT. L ) STOP 13

  L = .FALSE.
  IF ( ModFun(DT(-2)) .NE. -1.0 ) STOP 14

  END

