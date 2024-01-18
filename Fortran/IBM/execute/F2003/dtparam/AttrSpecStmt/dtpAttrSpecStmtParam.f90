!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtParam
!*
!*  DATE                       : Jun. 11, 2007
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
!*  -- PARAMETER statement
!*
!*  (ICE)
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
    INTEGER, KIND :: K1=1
    INTEGER, LEN  :: L1=1
    CONTAINS
    PROCEDURE :: ModFun1
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER, KIND :: K2=1
    INTEGER, LEN  :: L2=1
    INTEGER(K2)   :: I(L2)=K2
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

  FUNCTION ModFun2(Arg)
  CLASS(DT2(1,*,4,*,8,*)), INTENT(IN) :: Arg
  integer ModFun2(Arg%L2)
    ModFun2 = -Arg%I
  END FUNCTION

  END MODULE


  PROGRAM dtpAttrSpecStmtParam
  USE M

  TYPE(DT0(1,3)) :: T0
  PARAMETER ( T0 = DT0(1,3)() )
  TYPE(DT0(1,3)) :: T01

  TYPE(DT1(1,3,4,5)) :: T1
  PARAMETER ( T1 = DT1(1,3,4,5)() )

  TYPE(DT2(1,3,4,5,8,7)) :: T2
  PARAMETER ( T2 = DT2(1,3,4,5,8,7)(i = [1,2,3,4,5,6,7]) )
  INTEGER :: IArr(7)=0


  T01 = T0%ModFun0()

  IF ( ANY( T1%ModFun1() .NE. [(1,3),(4,5)] ) ) STOP 11

  IArr = T2%ModFun2()
  IF (   ANY( IArr .NE. [-1,-2,-3,-4,-5,-6,-7] ) ) STOP 12

  IF ( ANY( ModFun1(DT1(1,3,4,5)()) .NE. [(1,3),(4,5)] ) ) STOP 13

  END


