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
!*  R524 data-stmt is DATA data-stmt-set [ [ , ] data-stmt-set ] ...
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

  integer FUNCTION ModFun2(Arg)
  CLASS(DT2(1,*,4,*,8,*)), INTENT(IN) :: Arg
  dimension modfun2 (arg%l2)
    ModFun2 = -Arg%I
  END FUNCTION

  END MODULE


  PROGRAM dtpAttrSpecStmtData1
  USE M

  INTEGER, PARAMETER  :: N=17

  TYPE(DT0(1,3)) :: T0(N)
  TYPE(DT1(1,3,4,5)) :: T1(N)
  TYPE(DT2(1,3,4,5,8,7)) :: T2(N)

  TYPE(DT0(1,3)) :: T01(N)

  DATA T0  / N*DT0(1,3)() /, T1 / N*DT1(1,3,4,5)(DT0=DT0(1,3)(), r=0) /, &
                             T2 / N*DT2(1,3,4,5,8,7)(              &
                                  DT1=DT1(1,3,4,5)([1,2,3,4,5]),&
                                    I=[1,2,3,4,5,6,7],             &
                                    C=CHAR([1,2,3,4,5,6,7]),       &
                                  Ptr=NULL() )                     /

  INTEGER :: IArr(N)=0

  DO I=1, N

  T01 = T0(I)%ModFun0()

  IF ( ANY( T1(I)%ModFun1() .NE. [(1,3),(4,5)] ) ) ERROR STOP 11

  IF ( ANY( T2(I)%I .NE. [1,2,3,4,5,6,7]       ) ) ERROR STOP 12
  IF ( ANY( T2(I)%C .NE. CHAR([1,2,3,4,5,6,7]) ) ) ERROR STOP 13

  IArr(:7) = T2(I)%ModFun2()
  IF ( ANY( IArr(:7) .NE. [-1,-2,-3,-4,-5,-6,-7] ) ) ERROR STOP 14
  IF ( ANY( T2(I)%DT1%ModFun1() .NE. [(1,3),(4,5)] ) ) ERROR STOP 15

  END DO

  END
