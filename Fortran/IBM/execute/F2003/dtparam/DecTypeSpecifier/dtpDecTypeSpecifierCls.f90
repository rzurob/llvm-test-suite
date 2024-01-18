!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpDecTypeSpecifierCls
!*
!*  DATE                       : May. 18, 2007
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
!*  A polymorphic allocatable object may be allocated to be of any type
!*  with which it is type compatible.
!*  -- The dynamic type
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtpDecTypeSpecifierCls

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=0
    INTEGER, LEN  :: L0=0
  END TYPE

  TYPE, EXTENDS(DT0)  :: DT1(K1,L1)
    INTEGER(K0), KIND :: K1=2*K0
    INTEGER(K0), LEN  :: L1=2*K0
    INTEGER(K1)       :: I=K1
    CHARACTER(L0)     :: C(L1)="X"
  END TYPE

  CLASS(DT0), ALLOCATABLE :: T1
  CLASS(DT0(2,2)), ALLOCATABLE :: T2
  CLASS(DT1(1,1,K1=2,L1=2)), ALLOCATABLE :: T3

  ALLOCATE(T1, SOURCE=DT0())
  SELECT  TYPE( T1)
  TYPE IS( DT0(0,*))
    IF ( T1%K0          .NE.   0          ) STOP 11
    IF ( T1%L0          .NE.   0          ) STOP 12
  CLASS DEFAULT
    STOP 13
  END SELECT

  ALLOCATE(T2, SOURCE=DT1(2,2)())
  SELECT  TYPE( T2)
  TYPE IS( DT1(2, L0=*,K1=4,L1=*))
    IF ( T2%K0               .NE.   2          ) STOP 21
    IF ( T2%L0               .NE.   2          ) STOP 22
    IF ( T2%K1%KIND          .NE.   2          ) STOP 23
    IF ( T2%L1%KIND          .NE.   2          ) STOP 24
    IF ( T2%K1               .NE.   4          ) STOP 25
    IF ( T2%L1               .NE.   4          ) STOP 26
    IF ( T2%I%KIND           .NE.   4          ) STOP 27
    IF ( T2%I                .NE.   4          ) STOP 28
    IF ( T2%C%LEN            .NE.   2          ) STOP 29
    IF ( SIZE(T2%C)          .NE.   4          ) STOP 30
    IF ( ANY(T2%C            .NE.   "X"      ) ) STOP 31
  CLASS DEFAULT
    STOP 32
  END SELECT

  ALLOCATE(T3, SOURCE=DT1(1,1,2,2)(I=-1))
  SELECT  TYPE( T3)
  TYPE IS( DT1(1,L0=*,K1=2,L1=*))
    IF ( T3%K0               .NE.   1          ) STOP 41
    IF ( T3%L0               .NE.   1          ) STOP 42
    IF ( T3%K1%KIND          .NE.   1          ) STOP 43
    IF ( T3%L1%KIND          .NE.   1          ) STOP 44
    IF ( T3%K1               .NE.   2          ) STOP 45
    IF ( T3%L1               .NE.   2          ) STOP 46
    IF ( T3%I%KIND           .NE.   2          ) STOP 47
    IF ( T3%I                .NE.  -1          ) STOP 48
    IF ( T3%C%LEN            .NE.   1          ) STOP 49
    IF ( SIZE(T3%C)          .NE.   2          ) STOP 50
    IF ( ANY(T3%C            .NE.   "X "     ) ) STOP 51
  CLASS DEFAULT
    STOP 52
  END SELECT

  END

