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
!*  A data-stmt-constant with a repeat factor of zero contributes no data-stmt
!*  constants to the expanded sequence of scalar data-stmt-constants
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
    TYPE(DT1(K0,L0,K1,L1)), POINTER :: Ptr!=>NULL()
  END TYPE

  END MODULE


  PROGRAM dtpAttrSpecStmtData15
  USE M

  TYPE(DT0(1,3)),         PARAMETER :: C01=DT0(1,3)()
  TYPE(DT1(1,3,4,5)),     PARAMETER :: C11=DT1(1,3,4,5)(DT0=DT0(1,3)(),R=1)
  TYPE(DT2(1,3,4,5,8,7)), PARAMETER :: C21=DT2(1,3,4,5,8,7)(                   &
                                               DT1=DT1(1,3,4,5)(R=1),          &
                                                 I=1,                          &
                                                 C=CHAR(1),                    &
                                               Ptr=NULL() )

  TYPE(DT0(1,3)),         PARAMETER :: C02=DT0(1,3)()
  TYPE(DT1(1,3,4,5)),     PARAMETER :: C12=DT1(1,3,4,5)(DT0=DT0(1,3)(),R=2)
  TYPE(DT2(1,3,4,5,8,7)), PARAMETER :: C22=DT2(1,3,4,5,8,7)(                   &
                                               DT1=DT1(1,3,4,5)(R=2),          &
                                                 I=2,                          &
                                                 C=CHAR([1,2,3,4,5,6,7]),      &
                                               Ptr=NULL() )

  TYPE(DT0(1,3)),         PARAMETER :: C03=DT0(1,3)()
  TYPE(DT1(1,3,4,5)),     PARAMETER :: C13=DT1(1,3,4,5)(DT0=DT0(1,3)(),R=3)
  TYPE(DT2(1,3,4,5,8,7)), PARAMETER :: C23=DT2(1,3,4,5,8,7)(                   &
                                               DT1=DT1(1,3,4,5)(R=3),          &
                                                 I=3,                          &
                                                 C=CHAR(3),                    &
                                               Ptr=NULL() )

  INTEGER, PARAMETER  :: N=3
  INTEGER :: I,J

  TYPE(DT0(1,3))         :: S0(N,N)
  TYPE(DT1(1,3,4,5))     :: S1(N,N)
  TYPE(DT2(1,3,4,5,8,7)) :: S2(N,N)
  TYPE(DT2(1,3,4,5,8,7)) :: S3(N,N)


  DATA    S0  / 0 * C03, N * C01, N * C02, N * C03  /
  DATA    S1  / N * C11, 0 * C13, N * C12, N * C13  /
  DATA    S2  / 0 * C21, 0 * C22, 0 * C23  /
  DATA    S3  / N * C21, 0 * C23, N * C22, 0 * C21, N * C23  /


  INTEGER :: IArr(N)=0

  ! S1 and S2 are not affected, but I can not verify them
! DO I=1, N
! DO J=1, N

!   IF ( ANY( S1(I,J)%R .NE. S1%K1          ) ) STOP 11
!   IF ( ANY( S2(I,J)%R .NE. S1%K1          ) ) STOP 12
!   IF ( ANY( S2(I,J)%I .NE. S2%K2          ) ) STOP 13
!   IF ( ANY( S2(I,J)%C .NE. CHAR(S2%K2)    ) ) STOP 14

! END DO
! END DO

  ! "0 * C21" and "0 * C23" has no affect on S3
  DO I=1, N

    IF ( ANY( S2(I,1)%R .EQ. C21%R          ) ) STOP 21
    IF ( ANY( S2(I,2)%R .EQ. C22%R          ) ) STOP 22
    IF ( ANY( S2(I,3)%R .EQ. C23%R          ) ) STOP 23

    IF ( ANY( S2(I,1)%I .EQ. C21%I          ) ) STOP 24
    IF ( ANY( S2(I,2)%I .EQ. C22%I          ) ) STOP 25
    IF ( ANY( S2(I,3)%I .EQ. C23%I          ) ) STOP 26

    IF ( ANY( S2(I,1)%C .EQ. C21%C          ) ) STOP 27
    IF ( ANY( S2(I,2)%C .EQ. C22%C          ) ) STOP 28
    IF ( ANY( S2(I,3)%C .EQ. C23%C          ) ) STOP 29


    IF ( ANY( S3(I,1)%R .NE. C21%R          ) ) STOP 31
    IF ( ANY( S3(I,2)%R .NE. C22%R          ) ) STOP 32
    IF ( ANY( S3(I,3)%R .NE. C23%R          ) ) STOP 33

    IF ( ANY( S3(I,1)%I .NE. C21%I          ) ) STOP 41
    IF ( ANY( S3(I,2)%I .NE. C22%I          ) ) STOP 42
    IF ( ANY( S3(I,3)%I .NE. C23%I          ) ) STOP 43

    IF ( ANY( S3(I,1)%C .NE. C21%C          ) ) STOP 51
    IF ( ANY( S3(I,2)%C .NE. C22%C          ) ) STOP 52
    IF ( ANY( S3(I,3)%C .NE. C23%C          ) ) STOP 53

  END DO

  END

