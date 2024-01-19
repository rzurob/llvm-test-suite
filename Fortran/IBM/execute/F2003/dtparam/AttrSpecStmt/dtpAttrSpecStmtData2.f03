!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 14, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  -- DATA statement
!*  A variable that appears in a DATA statement and has not been typed previously may appear in a
!*  subsequent type declaration only if that declaration confirms the implicit typing.
!*
!*  (ice)
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
    INTEGER(K1)       :: R(L1)!=K1
    CONTAINS
    PROCEDURE :: ModFun1
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND :: K2=1
    INTEGER(K1), LEN  :: L2=1
    INTEGER(K2)   :: I(L2)!=K2
    CHARACTER(L2) :: C(L2)!=CHAR(K2)
    TYPE(DT2(K0,L0,K1,L0,K2,L2)), POINTER :: Ptr!=>NULL()
    CONTAINS
    PROCEDURE :: ModFun2
  END TYPE

  INTEGER, PARAMETER  :: N=2044

  CONTAINS

  FUNCTION ModFun0(Arg)
  CLASS(DT0(1,*)), INTENT(IN) :: Arg
  TYPE(DT0(1,Arg%L0)) ModFun0
    ModFun0 = Arg
  END FUNCTION

  FUNCTION ModFun1(Arg)
  CLASS(DT1(1,*,4,*)), INTENT(IN) :: Arg
  COMPLEX ::  ModFun1(2)
    ModFun1(1) =  (Arg%K0, Arg%L0)
    ModFun1(2) =  (Arg%K1, Arg%L1)
  END FUNCTION

  FUNCTION ModFun2(Arg)
  CLASS(DT2(1,*,4,*,8,*)), INTENT(IN) :: Arg
  TYPE(DT2(1,Arg%L0,4,Arg%L1,8,Arg%L2)) ModFun2
    IF ( SIZE( ModFun2%I ) .NE. Arg%L2 ) ERROR STOP 22
    ModFun2%I = -Arg%I
  END FUNCTION

  END MODULE


  PROGRAM dtpAttrSpecStmtData2
  USE M

  TYPE(DT0(1,3)) :: T0(N)
  TYPE(DT1(1,3,4,5)) :: T1(N)
  TYPE(DT2(1,3,4,5,8,7)) :: T2(N), t22

  DATA t0  / N*DT0(1,3)() /, t1 / N*DT1(1,3,4,5)(DT0=DT0(1,3)(),   &
                                               R=[1,2,3,4,5]) /
  data t2 / N*DT2(1,3,4,5,8,7)(              &
                                DT1=DT1(1,3,4,5)([1,2,3,4,5]),             &
                                  I=[1,2,3,4,5,6,7],             &
                                  C=CHAR([1,2,3,4,5,6,7]),       &
                                Ptr=NULL() )                     /

  TYPE(DT0(1,3)) :: T01(N)

  DO I=1, N

    T01 = T0(I)%ModFun0()

    IF ( ANY( T1(I)%R .NE. [1,2,3,4,5]                        ) ) ERROR STOP 10
    IF ( ANY( T1(I)%ModFun1() .NE. [(1,3),(4,5)] ) ) ERROR STOP 11

    IF ( ANY( T2(I)%I .NE. [1,2,3,4,5,6,7]       ) ) ERROR STOP 12
    IF ( ANY( T2(I)%C .NE. CHAR([1,2,3,4,5,6,7]) ) ) ERROR STOP 13

    t22 = T2(I)%ModFun2()
    IF ( ANY( t22%i .NE. [-1,-2,-3,-4,-5,-6,-7] ) ) ERROR STOP 14
    IF ( ANY( T2(I)%DT1%ModFun1() .NE. [(1,3),(4,5)] ) ) ERROR STOP 15

  END DO

  END

