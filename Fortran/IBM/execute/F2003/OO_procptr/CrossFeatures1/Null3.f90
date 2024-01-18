! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Null3.f 
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
!*  TEST CASE NAME             : Null3.f 
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
!*   Initialization/a structure constructor
!*  (305627) (306255)
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

  PROGRAM Null3 
  USE M
  IMPLICIT NONE 

  TYPE (DT), PARAMETER :: V=DT(-1, NULL())

  PROCEDURE(Fun), POINTER :: ProcPtr1=>NULL()
  PROCEDURE(Fun), POINTER :: ProcPtr2=>NULL()
  PROCEDURE(Fun), POINTER :: ProcPtr3=>NULL()
  PROCEDURE(Fun), POINTER :: ProcPtr4=>NULL()

  TYPE (DT) :: W1=DT(-1, NULL())
  TYPE (DT) :: W2(3)=DT(-1, NULL(V%ProcPtr)) 
  TYPE (DT) :: W3(3)=(/DT(-1, NULL()),DT(-1, NULL()),DT(-1, NULL()) /) 
  TYPE (DT) :: W4(1)=(/DT(-1, NULL()) /) 

  IF ( ASSOCIATED(ProcPtr1) ) STOP 11
  IF ( ASSOCIATED(ProcPtr2) ) STOP 12
  IF ( ASSOCIATED(ProcPtr3) ) STOP 13
  IF ( ASSOCIATED(ProcPtr4) ) STOP 14

  ProcPtr1 => Fun 
  ProcPtr2 => Fun 
  ProcPtr3 => Fun 
  ProcPtr4 => Fun
 
  ProcPtr1=>NULL()
  ProcPtr2=>NULL(ProcPtr1)
  ProcPtr3=>NULL(V%ProcPtr)
  ProcPtr4=>NULL(ProcPtr1)

  IF ( ASSOCIATED(ProcPtr1) ) STOP 21
  IF ( ASSOCIATED(ProcPtr2) ) STOP 22
  IF ( ASSOCIATED(ProcPtr3) ) STOP 23
  IF ( ASSOCIATED(ProcPtr4) ) STOP 24

  IF ( ASSOCIATED(W1%ProcPtr) )    STOP 31
  IF ( ASSOCIATED(W2(3)%ProcPtr) ) STOP 32
  IF ( ASSOCIATED(W3(1)%ProcPtr) ) STOP 33
  IF ( ASSOCIATED(W4(1)%ProcPtr) ) STOP 34

  W1%ProcPtr => Fun
  W2 = DT(-1, Fun) 
  W3 = DT(-1, Fun) 
  W4 = DT(-1, Fun) 

  W1 = DT(-1, NULL()) 
  W2 = DT(-1, NULL(W1%ProcPtr)) 
  W3 = (/DT(-1, NULL()),DT(-1, NULL()),DT(-1, NULL()) /) 
  W4 = (/DT(-1, NULL(W3(1)%ProcPtr)) /) 

  IF ( ASSOCIATED(W1%ProcPtr) )    STOP 41
  IF ( ASSOCIATED(W2(2)%ProcPtr) ) STOP 42
  IF ( ASSOCIATED(W3(3)%ProcPtr) ) STOP 43
  IF ( ASSOCIATED(W4(1)%ProcPtr) ) STOP 44


  END


