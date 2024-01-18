! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/OO_procptr/CrossFeatures2/Forall2.f
! opt variations: -ql

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: Forall2.f 
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
!*  TEST CASE NAME             : Forall2.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 21, 2005
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
!*  FORALL/defined operator 
!*  
!*  (ICE) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id
      PROCEDURE(Fun1), POINTER, NOPASS   :: ProcPtr=>NULL()
      !PROCEDURE(IFun), POINTER, PASS(Arg) :: ProcPtr1=>NULL()
      PROCEDURE(Fun), POINTER, PASS(Arg) :: ProcPtr1=>NULL()
    END TYPE

    INTERFACE OPERATOR ( + )   
      MODULE PROCEDURE OP
    END INTERFACE OPERATOR ( + )

    INTERFACE
      PURE FUNCTION IFun(Arg)
      IMPORT
      TYPE(DT(4)) :: IFun
      CLASS(DT(4)), INTENT(IN) :: Arg
      END FUNCTION
    END INTERFACE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE Assign
    END INTERFACE ASSIGNMENT ( = )
    CONTAINS

    PURE FUNCTION Op (Arg1, Arg2)
    TYPE (DT(4)), INTENT (IN) :: Arg1
    TYPE (DT(4)), INTENT (IN) :: Arg2
    TYPE (DT(4)) :: Op
      Op%Id = Arg1%Id + Arg2%Id 
      Op%ProcPtr => Arg2%ProcPtr
      Op%ProcPtr1 => Arg2%ProcPtr1
    END FUNCTION

    PURE SUBROUTINE Assign (Arg1, Arg2)
    TYPE (DT(4)), INTENT (OUT) :: Arg1
    TYPE (DT(4)), INTENT (IN)  :: Arg2
      Arg1%Id = Arg2%ID
      Arg1%ProcPtr => Arg2%ProcPtr
      Arg1%ProcPtr1 => Arg2%ProcPtr1
    END SUBROUTINE
 
    PURE FUNCTION Fun(Arg)
    TYPE(DT(4)) :: Fun
    CLASS(DT(4)), INTENT(IN) :: Arg
      Fun = Arg
    END FUNCTION

    PURE FUNCTION Fun1(Arg)
    INTEGER :: Fun1
    INTEGER, INTENT(IN) :: Arg
      Fun1 = Arg
    END FUNCTION

  END MODULE


  PROGRAM Forall2 
  USE M 
  IMPLICIT NONE 

  TYPE (DT(4)) :: V, W(30), U(30)
  INTEGER   :: I 
  PROCEDURE(IFun), POINTER :: ProcPtr

  V%Id = 1
  V%ProcPtr => Fun1
  V%ProcPtr1 => Fun
  ProcPtr => Fun 

  FORALL (I=V%ProcPtr(1):V%ProcPtr(30):V%ProcPtr(1))
    W(I) = ProcPtr(V) + ProcPtr(DT(4)(3,Fun1, Fun)) 
    U(I) = V%ProcPtr1() + V%ProcPtr1()
  END FORALL

  DO I=1, 30

    IF ( W(I)%Id .NE. 4 ) STOP 11
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun1) ) STOP 12
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr1, Fun) ) STOP 13
   
    IF ( U(I)%Id .NE. 2 ) STOP 21
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun1) ) STOP 22
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr1, Fun) ) STOP 23

  END DO

  FORALL (I=Fun1(1):Fun1(15)+Fun1(15):1)
    W(I) = DT(4)(1, V%ProcPtr, V%ProcPtr1)+ DT(4)(1, Fun1, Fun)
    U(I) = W(I) 
  END FORALL

  DO I=1, 30

    IF ( W(I)%Id .NE. 2 ) STOP 31
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr, Fun1) ) STOP 32
    IF ( .NOT. ASSOCIATED(W(I)%ProcPtr1, Fun) ) STOP 33
   
    IF ( W(I)%Id .NE. 2 ) STOP 41
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr, Fun1) ) STOP 42
    IF ( .NOT. ASSOCIATED(U(I)%ProcPtr1, Fun) ) STOP 43

  END DO

  END


