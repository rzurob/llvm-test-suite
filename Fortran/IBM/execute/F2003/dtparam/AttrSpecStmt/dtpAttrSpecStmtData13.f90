!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtData13
!*
!*  DATE                       : Jun. 21, 2007
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
!*
!*  The sequence of array elements is in array element order
!*  -- array section and implied do
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
    PROCEDURE, NOPASS :: ModFun2
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
  CLASS(DT2(1,*,4,*,8,*)), INTENT(IN) :: Arg(:,:)
  TYPE(DT2(1,Arg%L0,4,Arg%L1,8,Arg%L2)) ModFun2(SIZE(Arg,1),SIZE(Arg,2))
    ModFun2 = Arg
    DO I=1, Arg%L2
      ModFun2%I(I) = -Arg%I(I)
    END DO
    IF ( SIZE( ModFun2(1,1)%I ) .NE. Arg%L2 ) STOP 99
  END FUNCTION

  END MODULE


  PROGRAM dtpAttrSpecStmtData13
  USE M

  TYPE(DT0(1,3)),         PARAMETER :: C01=DT0(1,3)()
  TYPE(DT1(1,3,4,5)),     PARAMETER :: C11=DT1(1,3,4,5)(DT0=DT0(1,3)(),R=1)
  TYPE(DT2(1,3,4,5,8,7)), PARAMETER :: C21=DT2(1,3,4,5,8,7)(                   &
                                               DT1=DT1(1,3,4,5)(4),          &
                                                 I=1,                          &
                                                 C=CHAR(1),                    &
                                               Ptr=NULL() )

  TYPE(DT0(1,3)),         PARAMETER :: C02=DT0(1,3)()
  TYPE(DT1(1,3,4,5)),     PARAMETER :: C12=DT1(1,3,4,5)(DT0=DT0(1,3)(),R=2)
  TYPE(DT2(1,3,4,5,8,7)), PARAMETER :: C22=DT2(1,3,4,5,8,7)(                   &
                                               DT1=DT1(1,3,4,5)(4), &
                                                 I=2,                          &
                                                 C=CHAR([1,2,3,4,5,6,7]),      &
                                               Ptr=NULL() )

  TYPE(DT0(1,3)),         PARAMETER :: C03=DT0(1,3)()
  TYPE(DT1(1,3,4,5)),     PARAMETER :: C13=DT1(1,3,4,5)(DT0=DT0(1,3)(),R=3)
  TYPE(DT2(1,3,4,5,8,7)), PARAMETER :: C23=DT2(1,3,4,5,8,7)(                   &
                                               DT1=DT1(1,3,4,5)(4),  &
                                                 I=3,                          &
                                                 C=CHAR(3),                    &
                                               Ptr=NULL() )

  INTEGER, PARAMETER  :: N=3
  INTEGER :: I,J

  TYPE(Dt0(1,3))         :: S0(N,N)
  TYPE(Dt1(1,3,4,5))     :: S1(N,N)
  TYPE(Dt2(1,3,4,5,8,7)) :: S2(N,N), Arr(N,N)

  DATA    S0(:,1), S0(:,2), S0(:,3)  / N * C01, N * C02, N * C03  /
  DATA    S1(:,1), S1(:,2), S1(:,3)  / N * C11, N * C12, N * C13  /
  DATA    ((S2(I,J),I=1,N ),J=1,N)   / N * C21, N * C22, N * C23  /


  TYPE(Dt2(1,3,4,5,8,7)) :: IArr(N,n)

  IF ( ANY( S1(2,1)%R .NE. C11%R          ) ) STOP 21
  IF ( ANY( S1(3,2)%R .NE. C12%R          ) ) STOP 22
  IF ( ANY( S1(1,3)%R .NE. C13%R          ) ) STOP 23

  IF ( ANY( S2(3,1)%R .NE. C21%R          ) ) STOP 31
  IF ( ANY( S2(1,2)%R .NE. C22%R          ) ) STOP 32
  IF ( ANY( S2(2,3)%R .NE. C23%R          ) ) STOP 33

  IF ( ANY( S2(1,1)%I .NE. C21%I          ) ) STOP 41
  IF ( ANY( S2(2,2)%I .NE. C22%I          ) ) STOP 42
  IF ( ANY( S2(3,3)%I .NE. C23%I          ) ) STOP 43

  IF ( ANY( S2(3,1)%C .NE. C21%C          ) ) STOP 51
  IF ( ANY( S2(2,2)%C .NE. C22%C          ) ) STOP 52
  IF ( ANY( S2(1,3)%C .NE. C23%C          ) ) STOP 53


  DO I=1, N
  DO J=1, N

    IF ( ( S0(I,J)%ModFun0()              .NE. S0(I,J)%L0    ) ) STOP 60
    IF ( ANY( S1(I,J)%ModFun1() .NE. [(1,3),(4,5)] ) ) STOP 61

    IF (  ASSOCIATED(S2(I,J)%Ptr) )                STOP 62

    IArr = S2(I,J)%ModFun2(S2)
    IF ( ANY( IArr(3,1)%I .NE. -C21%I   ) ) STOP 63
    IF ( ANY( IArr(1,2)%I .NE. -C22%I   ) ) STOP 64
    IF ( ANY( IArr(2,3)%I .NE. -C23%I   ) ) STOP 65

  END DO
  END DO

  END

