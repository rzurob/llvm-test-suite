!*********************************************************************
!*  ===================================================================
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
!*     (constant  array)
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M


  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    CONTAINS
    PROCEDURE, NOPASS :: ModFun0
  END TYPE

  TYPE,  EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER, KIND :: K1=1
    INTEGER, LEN  :: L1=1
    CONTAINS
    PROCEDURE, NOPASS :: ModFun1
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER, KIND :: K2=1
    INTEGER, LEN  :: L2=1
    INTEGER(K2)   :: I(L2)=K2
    CONTAINS
    PROCEDURE, NOPASS :: ModFun2
  END TYPE


  CONTAINS

  FUNCTION ModFun0(Arg)
  CLASS(DT0(1,*)), INTENT(IN) :: Arg(:)
  TYPE(DT0(1,Arg%L0)) ModFun0(SIZE(Arg))
    ModFun0 = Arg
  END FUNCTION

  FUNCTION ModFun1(Arg)
  CLASS(DT1(1,*,4,*)), INTENT(IN) :: Arg(:)
  COMPLEX ::  ModFun1(SIZE(Arg),2)
    ModFun1(:,1) =  (Arg%K0, Arg%L0)
    ModFun1(:,2) =  (Arg%K1, Arg%L1)
  END FUNCTION

  FUNCTION ModFun2(Arg)
  CLASS(DT2(1,*,4,*,8,*)), INTENT(IN) :: Arg (:)
  TYPE(DT2(1,Arg%L0,4,Arg%L1,8,Arg%L2)) ModFun2(SIZE(Arg))
  INTEGER :: I
    DO I =1, SIZE(Arg)
      IF ( SIZE( ModFun2(I)%I ) .NE. Arg%L2 ) STOP 22
      ModFun2(I)%I = -Arg(I)%I
    END DO
  END FUNCTION

  END MODULE


  PROGRAM dtpAttrSpecStmtParam1
  USE M

  INTEGER        :: I

  TYPE(DT0(1,3)) :: T0
  DIMENSION      :: T0(15)
  PARAMETER ( T0 = [(DT0(1,3)(), I=1,15)] )
  TYPE(DT0(1,3)) :: T01(15)

  TYPE(DT1(1,3,4,5)) :: T1
  DIMENSION          :: T1(15)
  PARAMETER ( T1 = [(DT1(1,3,4,5)(),I=1,15)] )

  TYPE(DT2(1,3,4,5,8,7)) :: T2
  DIMENSION              :: T2(15)
  TYPE(DT2(1,3,4,5,8,7)) :: T22(15)
  INTEGER :: IArr(7)=0
  complex :: T11(15,2)

  T2 = [(DT2(1,3,4,5,8,7)(I),I=1,15)]

  T01 = T0%ModFun0(t0)

  T11 =  T1(1)%ModFun1(T1)
  IF ( ANY( T11(:,1) .NE. (1,3) ) ) STOP 11
  IF ( ANY( T11(:,2) .NE. (4,5) ) ) STOP 12

  T22 = T2%ModFun2( T2 )
  DO I = 1, 15
    IF (   ANY( T22(I)%I .NE. -i ) ) STOP 13
  END DO

  T11 = 0
  T11 = T2(1)%ModFun1 ( T2%DT1 )
  IF ( ANY( T11(:,1) .NE. (1,3) ) ) STOP 14
  IF ( ANY( T11(:,2) .NE. (4,5) ) ) STOP 15

  END


