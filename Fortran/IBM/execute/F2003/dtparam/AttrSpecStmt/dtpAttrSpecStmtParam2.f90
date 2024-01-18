!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtParam2
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 14, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!* 
!*  -- PARAMETER statement and implicit typeing
!* 
!*  ()
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    CONTAINS
    PROCEDURE :: ModFun0 
  END TYPE

  TYPE,  EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND :: K1=1
    INTEGER(K0), LEN  :: L1=1
    INTEGER(K1)       :: R(L1)=K1
    CONTAINS
    PROCEDURE :: ModFun1 
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND :: K2=1
    INTEGER(K1), LEN  :: L2=1
    INTEGER(K2)   :: I(L2)=K2
    CHARACTER(L2) :: C(L2)=CHAR(K2)
    TYPE(DT2(K0,L0,K1,L0,K2,L2)), POINTER :: Ptr=>NULL()
    CONTAINS
    PROCEDURE :: ModFun2 
  END TYPE

  INTEGER, PARAMETER  :: N=4096

  CONTAINS

  FUNCTION ModFun0(Arg)
  CLASS(DT0(1,*)), INTENT(IN) :: Arg 
  TYPE(DT0(1,Arg%L0)) ModFun0
    ModFun0 = Arg 
  END FUNCTION 

  FUNCTION ModFun1(Arg)
  CLASS(DT1(1,*,4,*)), INTENT(IN) :: Arg 
  COMPLEX ::  ModFun1(2)
    ModFun1(1) =  cmplx(Arg%K0, Arg%L0)
    ModFun1(2) =  cmplx(Arg%K1, Arg%L1)
  END FUNCTION 

  FUNCTION ModFun2(Arg)
  CLASS(DT2(1,*,4,*,8,*)), INTENT(IN) :: Arg 
  TYPE(DT2(1,Arg%L0,4,Arg%L1,8,Arg%L2)) ModFun2
    ModFun2%I = -Arg%I 
    IF ( SIZE( ModFun2%I ) .NE. Arg%L2 ) STOP 22
  END FUNCTION 

  END MODULE


  PROGRAM dtpAttrSpecStmtParam2
  USE M
  IMPLICIT type(DT0(1,3))(O)
  IMPLICIT type(DT1(1,3,4,5))(P)
  IMPLICIT type(DT2(1,3,4,5,8,7))(Q)

  DIMENSION :: O(N)
  DIMENSION :: P(N)
  DIMENSION :: Q(N)

  PARAMETER ( O=DT0(1,3)() )
  PARAMETER ( P=DT1(1,3,4,5)(DT0=O(1),R=[1,2,3,4,5]) )
  PARAMETER ( Q=DT2(1,3,4,5,8,7)(                               &
                                DT1=P(1),                          &
                                  I=[1,2,3,4,5,6,7],            &
                                  C=CHAR([1,2,3,4,5,6,7]),      &
                                Ptr=NULL()                     ))

  TYPE(DT0(1,3)) :: T01(N)

  !TYPE(DT0(1,3)) :: O(N)
  !TYPE(DT1(1,3,4,5)) :: P(N)
  !TYPE(DT2(1,3,4,5,8,7)) :: Q(N)

  DO I=1, N

    T01 = o0%ModFun0()

    IF ( ANY( p(I)%R .NE. [1,2,3,4,5]                     ) ) STOP 10
    IF ( ANY( p(I)%ModFun1() .NE. [(1,3),(4,5)] ) ) STOP 11

    IF ( ANY( q(I)%I .NE. [1,2,3,4,5,6,7]       ) ) STOP 12
    IF ( ANY( q(I)%C .NE. CHAR([1,2,3,4,5,6,7]) ) ) STOP 13

    IF ( ANY( ModFun1(DT1(1,3,4,5)()) .NE. [(1,3),(4,5)] ) ) STOP 15

  END DO

  END


