! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Allocate3.f 
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
!*  TEST CASE NAME             : Allocate3.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 9, 2005
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
!*  The allocate stmt 
!*   
!*  (306345) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    INTERFACE 
      FUNCTION IntF(Arg)
        PROCEDURE(INTEGER), POINTER :: IntF 
        PROCEDURE(INTEGER)          :: Arg
      END FUNCTION
    END INTERFACE
    
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(IntF),NOPASS,  POINTER :: ProcPtr1=>NULL()
      PROCEDURE(INTEGER),NOPASS,  POINTER :: ProcPtr2=>NULL()
    END TYPE
    CONTAINS

    FUNCTION ProcFun(Arg)
    PROCEDURE(INTEGER), POINTER :: ProcFun
    PROCEDURE(INTEGER)          :: Arg
      ProcFun => Arg 
    END FUNCTION

    FUNCTION Fun(Arg)
    INTEGER  :: Fun
    INTEGER  :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Allocate3 
  USE M
  IMPLICIT NONE 
  TYPE(DT)              :: V
  TYPE(DT), POINTER     :: VP
  TYPE(DT), ALLOCATABLE :: VA
 
  ALLOCATE(VP)
  IF ( .NOT. ASSOCIATED( VP ) )        STOP 11
  IF (       ASSOCIATED( VP%ProcPtr2)) STOP 12

  VP%ProcPtr2 => Fun
  IF ( .NOT. ASSOCIATED( VP%ProcPtr2 ))    STOP 13
  V = DT(VP%ProcPtr2(-1), NULL(), VP%ProcPtr2 ) 
  IF ( V%Id .NE. -1 )                      STOP 14
  IF ( .NOT. ASSOCIATED(V%ProcPtr2, Fun) ) STOP 15

  NULLIFY(VP%ProcPtr2)
  DEALLOCATE(VP)

  ALLOCATE(VA)
  IF ( .NOT. ALLOCATED( VA ))           STOP 21
  IF (       ASSOCIATED( VA%ProcPtr1 )) STOP 22

  VA%ProcPtr1 => ProcFun
  IF ( .NOT. ASSOCIATED( VA%ProcPtr1, ProcFun ))  STOP 23
  VA = DT(-2, VA%ProcPtr1, NULL())
  IF ( VA%Id .NE. -2 ) STOP 24
  IF ( .NOT. ASSOCIATED( VA%ProcPtr1, ProcFun ))  STOP 25

  NULLIFY(VA%ProcPtr2)
  DEALLOCATE(VA)

  END

 
