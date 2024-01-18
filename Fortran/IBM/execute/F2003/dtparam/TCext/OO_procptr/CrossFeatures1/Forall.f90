! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures1/Forall.f
! opt variations: -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Forall.f 
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
!*  TEST CASE NAME             : Forall.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 12, 2005
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
!*  FORALL/HEADER 
!*  
!*  (ICE-304566) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun), POINTER, NOPASS :: ProcPtr=>NULL()
    END TYPE

    CONTAINS

    FUNCTION Fun(Arg)
    INTEGER :: Fun
    INTEGER :: Arg
      Fun = Arg
    END FUNCTION

  END MODULE

  PROGRAM Forall 
  USE M
  IMPLICIT NONE 

  TYPE (DT(4)) :: V, W(30000), U(30000)
  INTEGER :: I
  PROCEDURE(Fun), POINTER :: ProcPtr

  V%Id = -1
  V%ProcPtr => Fun
  ProcPtr => Fun
 
  FORALL (I=V%ProcPtr(1):ProcPtr(30000):V%ProcPtr(1))
    W(I) = V
    U(I)%ProcPtr => ProcPtr
  END FORALL

  DO I=1, 30000

    IF ( W(I)%Id .NE. -1 ) STOP 11
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr) ) STOP 12
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun) ) STOP 13
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, ProcPtr) ) STOP 14
   
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr) ) STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun) ) STOP 23
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, ProcPtr) ) STOP 24

  END DO

  FORALL (I=ProcPtr(1):ProcPtr(30000):ProcPtr(1))
    W(I) = DT(4)(-1, PRocPtr) 
    U(I) = W(I) 
  END FORALL

  DO I=1, 30000

    IF ( W(I)%Id .NE. -1 ) STOP 31
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr) ) STOP 32
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun) ) STOP 33
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, ProcPtr) ) STOP 34
   
    IF ( W(I)%Id .NE. -1 ) STOP 41
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr) ) STOP 42
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun) ) STOP 43
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, ProcPtr) ) STOP 44

  END DO

  END


