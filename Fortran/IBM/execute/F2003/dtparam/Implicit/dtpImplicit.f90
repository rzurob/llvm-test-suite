!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpImplicit
!*
!*  DATE                       : Jun. 22, 2007
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
!*  -- The implicit statement
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
    INTEGER(K0), KIND :: K1=1
    INTEGER(K0), LEN  :: L1=1
    REAL(K1) :: R(L1)=K1
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

  CONTAINS

  FUNCTION ModFun0(Arg)
  CLASS(DT0(1,*)), INTENT(IN) :: Arg
  INTEGER ModFun0
    ModFun0 = Arg%L0
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
    ModFun2%R = -Arg%R
    ModFun2%I = -Arg%I
    ModFun2%C = CHAR(Arg%L2)
    IF ( SIZE( ModFun2%I ) .NE. Arg%L2 ) STOP 22
  END FUNCTION

  END MODULE

  PROGRAM dtpImplicit
  USE M
  IMPLICIT TYPE(DT0(1,3))(R)
  IMPLICIT TYPE(DT1(1,3,4,5))(S)
  IMPLICIT TYPE(DT2(1,3,4,5,8,7))(T)


  INTEGER :: IArr(7)=0

  IF ( R%ModFun0() .NE. R%L0 ) STOP 11

  IF ( SIZE( S%R )      .NE. S%L1          )  STOP 12
  IF ( S%R%KIND         .NE. S%K1            ) STOP 13
  IF ( ANY ( S%R        .NE. S%K1          ) ) STOP 14
  IF ( ANY( S%ModFun1() .NE. [(1,3),(4,5)] ) ) STOP 15

  IF ( SIZE( T%R )      .NE. T%L1       )  STOP 21
  IF ( T%R%KIND         .NE. T%K1         ) STOP 22
  IF ( ANY ( T%R        .NE. T%K1       ) ) STOP 23
  IF ( SIZE( T%I )      .NE. T%L2       )  STOP 24
  IF ( T%I%KIND         .NE. T%K2         ) STOP 25
  IF ( ANY ( T%I        .NE. T%K2       ) ) STOP 26
  IF ( SIZE( T%C )      .NE. T%L2       )  STOP 27
  IF ( T%C%LEN          .NE. T%l2         ) STOP 28
  IF ( ANY (T%C         .NE. CHAR(T%K2) ) ) STOP 29

  IF ( T%ModFun0()   .NE. T%L0            ) STOP 31
  IF ( any(T%ModFun1()   .NE. [(1,3),(4,5)])   ) STOP 32
  TT = T%ModFun2( )
  IF ( ANY ( TT%R    .NE. -T2%R       ) ) STOP 33
  IF ( ANY ( TT%I    .NE. -T2%I       ) ) STOP 34
  IF ( ANY ( TT%C    .NE. CHAR(T%L2)  ) ) STOP 35


  R =  DT0(1,3)()
  S =  DT1(1,3,4,5)([1,2,3,4,5])
  T =  DT2(1,3,4,5,8,7)(                  &
           DT1=DT1(1,3,4,5)([1,2,3,4,5]), &
             I=[1,2,3,4,5,6,7],           &
             C=CHAR([1,2,3,4,5,6,7]),     &
            Ptr=NULL() )


  IF ( R%ModFun0() .NE. R%L0 ) STOP 41

  IF ( ANY ( S%R         .NE. [1,2,3,4,5]   ) ) STOP 42
  IF ( ANY ( S%ModFun1() .NE. [(1,3),(4,5)] ) ) STOP 43

  IF ( ANY ( T%R        .NE. [1,2,3,4,5]           ) ) STOP 44
  IF ( ANY ( T%I        .NE. [1,2,3,4,5,6,7]       ) ) STOP 45
  IF ( ANY ( T%C        .NE. CHAR([1,2,3,4,5,6,7]) ) ) STOP 46


  END

