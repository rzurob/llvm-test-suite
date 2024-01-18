!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr2
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 22, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!* 
!*  -- the asynchronous attribute 
!* 
!*  ()
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=0
    INTEGER, LEN  :: L0=0
  END TYPE

  TYPE, EXTENDS(DT0) :: DT1(K1,L1)
    INTEGER(K0), KIND :: K1=2*K0
    INTEGER(K0), LEN  :: L1=2*K0
    INTEGER(K1),   PRIVATE,  ALLOCATABLE  :: I(:)
    CHARACTER(L0), PUBLIC,   ALLOCATABLE  :: C(:)
  END TYPE

  TYPE(DT0), ALLOCATABLE, ASYNCHRONOUS :: P1(:)
  TYPE(DT1(2,2)), ALLOCATABLE, ASYNCHRONOUS :: P2(:)
  TYPE(DT1(1,1,K1=2,L1=2)), ALLOCATABLE, ASYNCHRONOUS :: P3(:)

  TYPE(DT0), ASYNCHRONOUS          :: T1(107) 
  TYPE(DT1(2,2)), ASYNCHRONOUS     :: T2(107) 
  TYPE(DT1(1,1,2,2)), ASYNCHRONOUS :: T3(107)

  CONTAINS

  SUBROUTINE Check()
  INTEGER I

  DO I = 1, 107

    IF ( P1(I)%K0               .NE.   0          ) STOP 11
    IF ( P1(I)%L0               .NE.   0          ) STOP 12

    IF ( P2(I)%K0               .NE.   2          ) STOP 21
    IF ( P2(I)%L0               .NE.   2          ) STOP 22
    IF ( P2(I)%K1%KIND          .NE.   2          ) STOP 23
    IF ( P2(I)%L1%KIND          .NE.   2          ) STOP 24
    IF ( P2(I)%K1               .NE.   4          ) STOP 25
    IF ( P2(I)%L1               .NE.   4          ) STOP 26
    IF ( P2(I)%I%KIND           .NE.   4          ) STOP 27
    IF ( SIZE(P2(I)%I)          .NE.   1          ) STOP 28
    IF ( ANY(P2(I)%I            .NE.  -1        ) ) STOP 29
    IF ( P2(I)%C%LEN            .NE.   2          ) STOP 30
    IF ( SIZE(P2(I)%C)          .NE.   1          ) STOP 31
    IF ( ANY(P2(I)%C            .NE.   "X"      ) ) STOP 32

    IF ( P3(I)%K0               .NE.   1          ) STOP 41
    IF ( P3(I)%L0               .NE.   1          ) STOP 42
    IF ( P3(I)%K1%KIND          .NE.   1          ) STOP 43
    IF ( P3(I)%L1%KIND          .NE.   1          ) STOP 44
    IF ( P3(I)%K1               .NE.   2          ) STOP 45
    IF ( P3(I)%L1               .NE.   2          ) STOP 46
    IF ( P3(I)%I%KIND           .NE.   2          ) STOP 47
    IF ( SIZE(P3(I)%I)          .NE.   2          ) STOP 48
    IF ( ANY(P3(I)%I            .NE.  -1        ) ) STOP 49
    IF ( P3(I)%C%LEN            .NE.   1          ) STOP 50
    IF ( SIZE(P3(I)%C)          .NE.   2          ) STOP 51
    IF ( ANY(P3(I)%C            .NE.   "X"      ) ) STOP 52

  END DO

  END SUBROUTINE

  SUBROUTINE Assgn()

  T1 = DT0()
  T2 = DT1(2,2)(I=[-1], C=["X"])
  T3 = DT1(1,1,2,2)(I=[-1,-1],C=["X","X"])

  P1 = T1
  P2 = T2
  P3 = T3
     
  END SUBROUTINE

  END MODULE

  PROGRAM dtpObjDecAttr2
  USE M
  
  CALL Assgn()
  CALL Check()

  END

