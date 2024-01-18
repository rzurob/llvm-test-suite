! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Data4.f 
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
!*  TEST CASE NAME             : Data4.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 22, 2005
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
!*  Initialize proc-ptr component of an object in data stmt.
!* 
!*  (314894/315287) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id
      PROCEDURE(Fun), PASS, POINTER :: ProcPtr1
      PROCEDURE(Fun), PASS, POINTER :: ProcPtr2
      PROCEDURE(Fun), PASS, POINTER :: ProcPtr3
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    TYPE(DT)  :: Fun
    CLASS(DT) :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE


  PROGRAM Data4 
  USE M
  IMPLICIT NONE 

  INTEGER :: I

  TYPE (DT) :: V(3)
  DATA V /3*DT(-1,NULL(),NULL(),NULL())/  

  TYPE (DT) :: U(3)
  DATA (U(I)%Id, I=1,3 ) /3*-1/  
  DATA (U(I)%ProcPtr1, I=1,3 ) /3*NULL()/  
  DATA (U(I)%ProcPtr2, I=1,3 ) /3*NULL()/  
  DATA (U(I)%ProcPtr3, I=1,3 ) /3*NULL()/  

  TYPE (DT) :: W(3)
  DATA (W(I), I=1,3,2 ) /2*DT(-1, NULL(),NULL(),NULL())/  
  DATA (W(I), I=2,3,2 ) /1*DT(-1, NULL(),NULL(),NULL())/  


  IF (ASSOCIATED(V(1)%ProcPtr1)) STOP 11
  IF (ASSOCIATED(V(2)%ProcPtr2)) STOP 11
  IF (ASSOCIATED(V(3)%ProcPtr3)) STOP 11
  IF (ANY(V%Id .NE. -1))         STOP 12
   
  IF (ASSOCIATED(U(1)%ProcPtr1)) STOP 21
  IF (ASSOCIATED(U(2)%ProcPtr2)) STOP 21
  IF (ASSOCIATED(U(3)%ProcPtr3)) STOP 21
  IF (ANY(U%Id .NE. -1))         STOP 22
   
  IF (ASSOCIATED(W(1)%ProcPtr1)) STOP 31
  IF (ASSOCIATED(W(2)%ProcPtr2)) STOP 31
  IF (ASSOCIATED(W(3)%ProcPtr3)) STOP 31
  IF (ANY(W%Id .NE. -1))         STOP 32
   
  END


