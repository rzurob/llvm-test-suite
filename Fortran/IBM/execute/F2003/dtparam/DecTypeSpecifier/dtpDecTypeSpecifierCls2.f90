!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpDecTypeSpecifierCls2
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
!*  A polymorphic dummy is associated with the actual argument
!*  with which it is type compatible
!*
!*  -- The dynamic type
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtpDecTypeSpecifierCls2

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


  TYPE(DT0), TARGET :: T1=DT0()
  TYPE(DT1(2,2)), TARGET :: T2=DT1(2,2)()
  TYPE(DT1(1,1,2,2)), TARGET :: T3 = DT1(1,1,2,2)(I=-1)

  CALL S1(T1)
  CALL S2(T2)
  CALL S3(T3)

  CONTAINS

  SUBROUTINE S1(P1)
  CLASS(DT0) :: P1

  SELECT  TYPE( P1)
  TYPE IS( DT0(0,*))
    IF ( P1%K0          .NE.   0          ) STOP 11
    IF ( P1%L0          .NE.   0          ) STOP 12
  CLASS DEFAULT
    STOP 13
  END SELECT

  END SUBROUTINE


  SUBROUTINE S2(P2)
  CLASS(DT0(2,2)) :: P2

  SELECT  TYPE( P2)
  TYPE IS( DT1(2, L0=*,K1=4,L1=*))
    IF ( P2%K0               .NE.   2          ) STOP 21
    IF ( P2%L0               .NE.   2          ) STOP 22
    IF ( P2%K1%KIND          .NE.   2          ) STOP 23
    IF ( P2%L1%KIND          .NE.   2          ) STOP 24
    IF ( P2%K1               .NE.   4          ) STOP 25
    IF ( P2%L1               .NE.   4          ) STOP 26
    IF ( P2%I%KIND           .NE.   4          ) STOP 27
    IF ( P2%I                .NE.   4          ) STOP 28
    IF ( P2%C%LEN            .NE.   2          ) STOP 29
    IF ( SIZE(P2%C)          .NE.   4          ) STOP 30
    IF ( ANY(P2%C            .NE.   "X"      ) ) STOP 31
  CLASS DEFAULT
    STOP 32
  END SELECT

  END SUBROUTINE


  SUBROUTINE S3(P3)
  CLASS(DT1(1,1,K1=2,L1=2)) :: P3

  SELECT  TYPE( P3)
  TYPE IS( DT1(1,L0=*,K1=2,L1=*))
    IF ( P3%K0               .NE.   1          ) STOP 41
    IF ( P3%L0               .NE.   1          ) STOP 42
    IF ( P3%K1%KIND          .NE.   1          ) STOP 43
    IF ( P3%L1%KIND          .NE.   1          ) STOP 44
    IF ( P3%K1               .NE.   2          ) STOP 45
    IF ( P3%L1               .NE.   2          ) STOP 46
    IF ( P3%I%KIND           .NE.   2          ) STOP 47
    IF ( P3%I                .NE.  -1          ) STOP 48
    IF ( P3%C%LEN            .NE.   1          ) STOP 49
    IF ( SIZE(P3%C)          .NE.   2          ) STOP 50
    IF ( ANY(P3%C            .NE.   "X "     ) ) STOP 51
  CLASS DEFAULT
    STOP 52
  END SELECT

  END SUBROUTINE

  END

