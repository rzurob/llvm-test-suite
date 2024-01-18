!*********************************************************************
!*  ===================================================================
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
!*  A zero-sized array or a data-implied-do with an iteration count of zero contributes
!*  no variables to the expanded sequence of variables
!*
!*  (ice)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE,  EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND :: K1=1
    INTEGER(K0), LEN  :: L1=1
    INTEGER(K1)       :: R(L1)!=K1
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND :: K2=1
    INTEGER(K1), LEN  :: L2=1
    INTEGER(K2)   :: I(L2)!=K2
    CHARACTER(L2) :: C(L2)!=CHAR(K2)
    TYPE(DT2(K0,L0,K1,L1,K2,L2)), POINTER :: Ptr!=>NULL()
  END TYPE

  END MODULE


  PROGRAM dtpAttrSpecStmtData14
  USE M

  TYPE(DT0(1,3)),         PARAMETER :: C01=DT0(1,3)()
  TYPE(DT1(1,3,4,5)),     PARAMETER :: C11=DT1(1,3,4,5)(DT0=DT0(1,3)(),R=1)
  TYPE(DT2(1,3,4,5,8,7)), PARAMETER :: C21=DT2(1,3,4,5,8,7)(                   &
                                               DT1=DT1(1,3,4,5)(1),            &
                                                 I=1,                          &
                                                 C=CHAR(1),                    &
                                               Ptr=NULL() )

  TYPE(DT0(1,3)),         PARAMETER :: C02=DT0(1,3)()
  TYPE(DT1(1,3,4,5)),     PARAMETER :: C12=DT1(1,3,4,5)(DT0=DT0(1,3)(),R=2)
  TYPE(DT2(1,3,4,5,8,7)), PARAMETER :: C22=DT2(1,3,4,5,8,7)(                   &
                                               DT1=DT1(1,3,4,5)(1),            &
                                                 I=2,                          &
                                                 C=CHAR([1,2,3,4,5,6,7]),      &
                                               Ptr=NULL() )

  TYPE(DT0(1,3)),         PARAMETER :: C03=DT0(1,3)()
  TYPE(DT1(1,3,4,5)),     PARAMETER :: C13=DT1(1,3,4,5)(DT0=DT0(1,3)(),R=3)
  TYPE(DT2(1,3,4,5,8,7)), PARAMETER :: C23=DT2(1,3,4,5,8,7)(                   &
                                               DT1=DT1(1,3,4,5)(1),            &
                                                 I=3,                          &
                                                 C=CHAR(3),                    &
                                               Ptr=NULL() )

  INTEGER, PARAMETER  :: N=31
  INTEGER :: I,J

  TYPE(DT0(1,3))         :: S0(N,N)
  TYPE(DT1(1,3,4,5))     :: S1(N,N)
  TYPE(DT2(1,3,4,5,8,7)) :: S2(N,N), Arr(N,N)


  DATA    S0(:,1), S0(:,2), S0(1,3)  / N * C01, N * C02, N * C03  /
  DATA    S1(:,1), S1(:,2), S1(:,3)  / N * C11, N * C12, N * C13  /
  DATA    ((S2(I,J),i=1,n ),j=1,3)         / N * C21, N * C22, N * C23  /


  INTEGER :: IArr(N)=0


  DO I=1, N

    IF ( ANY( S1(I,1)%R /= C11%R          ) ) STOP 21
    IF ( ANY( S1(I,2)%R /= C12%R          ) ) STOP 22
    IF ( ANY( S1(I,3)%R /= C13%R          ) ) STOP 23

    IF ( ANY( S2(I,1)%R /= C21%R          ) ) STOP 31
    IF ( ANY( S2(I,2)%R /= C22%R          ) ) STOP 32
    IF ( ANY( S2(I,3)%R /= C23%R          ) ) STOP 33

    IF ( ANY( S2(I,1)%I /= C21%I          ) ) STOP 41
    IF ( ANY( S2(I,2)%I /= C22%I          ) ) STOP 42
    IF ( ANY( S2(I,3)%I /= C23%I          ) ) STOP 43

    IF ( ANY( S2(I,1)%C /= C21%C          ) ) STOP 51
    IF ( ANY( S2(I,2)%C /= C22%C          ) ) STOP 52
    IF ( ANY( S2(I,3)%C /= C23%C          ) ) STOP 53

  END DO

  END

