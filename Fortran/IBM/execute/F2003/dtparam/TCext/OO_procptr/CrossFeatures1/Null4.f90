! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/Null4.f
! opt variations: -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Null4.f 
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
!*  TEST CASE NAME             : Null4.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 11, 2005
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
!*   Actual argument/DATA statement 
!*  (same to 306255) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun), POINTER, NOPASS :: ProcPtr
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Null4 
  USE M
  IMPLICIT NONE 

  TYPE (DT(4)), PARAMETER :: V=DT(4)(-1, NULL())

  PROCEDURE(Fun), POINTER :: ProcPtr1
  PROCEDURE(Fun), POINTER :: ProcPtr2
  PROCEDURE(Fun), POINTER :: ProcPtr3
  PROCEDURE(Fun), POINTER :: ProcPtr4

  DATA  ProcPtr1 / NULL()/,           &
      & ProcPtr2 / NULL()/,           & 
      & ProcPtr3 / NULL()/,           &
      & ProcPtr4 / NULL()/  

  TYPE(DT(4)) :: W1,W2, W3(3), W4(1)

  DATA   W1 /DT(4)(-1, NULL())/,                                    &
       & W2 /DT(4)(-1, NULL())/ ,                                   &
       & W3 /DT(4)(-1, NULL()),DT(4)(-1, NULL()),DT(4)(-1, NULL()) /,     &
       & W4 /DT(4)(-1, NULL()) /


  IF ( ASSOCIATED(ProcPtr1) )      STOP 11
  IF ( ASSOCIATED(ProcPtr2) )      STOP 12
  IF ( ASSOCIATED(ProcPtr3) )      STOP 13
  IF ( ASSOCIATED(ProcPtr4) )      STOP 14


  IF ( ASSOCIATED(W1%ProcPtr) )     STOP 41
  IF ( ASSOCIATED(W2%ProcPtr) )     STOP 42
  IF ( ASSOCIATED(W3(3)%ProcPtr) )  STOP 43
  IF ( ASSOCIATED(W4(1)%ProcPtr) )  STOP 44


  CALL IntSub(NULL(), DT(4)(-1, NULL()), DT(4)(-1, NULL(V%ProcPtr)), NULL(ProcPtr1))
  
  CALL IntSub1(DT(4)(-1, NULL()),                                    &
              & DT(4)(-1, NULL(V%ProcPtr)),                          &
              &(/DT(4)(-1, NULL()),DT(4)(-1, NULL()),DT(4)(-1, NULL()) /), &
              & (/DT(4)(-1, NULL(W3(1)%ProcPtr)) /))
  
  CONTAINS

  SUBROUTINE IntSub(Arg1, Arg2, Arg3, Arg4)
  PROCEDURE(Fun), POINTER :: Arg1, Arg4
  TYPE (DT(4)) ::Arg2, Arg3 

  IF ( ASSOCIATED(Arg1) )         STOP 21
  IF ( ASSOCIATED(Arg2%ProcPtr) ) STOP 22
  IF ( ASSOCIATED(Arg3%ProcPtr) ) STOP 22
  IF ( ASSOCIATED(Arg4) )         STOP 24

  END SUBROUTINE

  SUBROUTINE IntSub1(Arg1, Arg2, Arg3, Arg4)
  TYPE (DT(4)) ::Arg1, Arg2, Arg3(:), Arg4(1) 

  IF ( ASSOCIATED(Arg1%ProcPtr) )    STOP 31
  IF ( ASSOCIATED(Arg2%ProcPtr) )    STOP 32
  IF ( ASSOCIATED(Arg3(1)%ProcPtr) ) STOP 33
  IF ( ASSOCIATED(Arg4(1)%ProcPtr) ) STOP 34

  END SUBROUTINE

  END


