!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtDim
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
!*  -- DIMENSION statement
!*
!*  (ice)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  DIMENSION              :: T0(:), T2(:)
  DIMENSION              :: T1(:)
  POINTER                :: T0, T2
  POINTER                :: T1


  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND    :: K1=K0
    INTEGER(K0), LEN     :: L1=K0
    CHARACTER(L1+3)      :: C1 = "DT1"
    CONTAINS
    PROCEDURE(ModFun0), NOPASS, DEFERRED :: Proc
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND    :: K2=K1
    INTEGER(K1), LEN     :: L2=K1
    CHARACTER(L2)        :: C2=CHAR(K2)
    INTEGER(K2)          :: I=K2
    REAL   (K2)          :: R=K2
    LOGICAL(K2)          :: L=.TRUE._1
    COMPLEX(K2)          :: Z=CMPLX(K1, K2, K2)
    TYPE(DT0(K2, L2))    :: T0(L2)
    TYPE(DT2(K0,L0,K1,L1,K2, L2)), POINTER  :: Ptr
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun0
  END TYPE

  TYPE(DT0(1,3))         :: T0
  TYPE(Dt2(1,3,4,5,8,7)) :: T2
  CLASS(DT1(1,:, 4,:))   :: T1


  TYPE(DT2(1,3,4,5,8,7)), PARAMETER ::             &
        CT =  DT2(1,3,4,5,8,7)   (                 &
                                  C1 = "XYZ",      &
                                  C2 = "ZYX",      &
                                   I = 1234,       &
                                   R = 4321.,      &
                                   L = .TRUE.,     &
                                   Z = (1.,-1.),   &
                                   Ptr  = NULL(),  &
                                   T0=DT0(8,7)() )

  CONTAINS

  FUNCTION ModFun0(Arg)
  CLASS(DT0(1,1)), TARGET, INTENT(IN)  :: Arg(:)
  CLASS(DT0(1,1)), POINTER             :: ModFun0(:)
    ModFun0 => Arg
  END FUNCTION

  SUBROUTINE ModSub(L0, L1, L2, N)
  IMPLICIT NONE

  INTEGER L0
  INTEGER L1
  INTEGER L2
  INTEGER N

  ALLOCATE(T0(N), SOURCE=DT0(1,3)())
  ALLOCATE(T1(N), SOURCE=CT)
  ALLOCATE(T2(N), SOURCE=CT)

  END SUBROUTINE

  END MODULE


  PROGRAM dtpAttrSpecStmtDim
  USE M
  IMPLICIT NONE
  INTEGER I

  INTEGER L0
  INTEGER L1
  INTEGER L2
  INTEGER N

  L0 = 3; L1 = 5; L2 = 7; N = 64
  CALL ModSub(L0, L1, L2, N)

  IF ( .NOT. ASSOCIATED(T0) ) STOP 24
  IF ( .NOT. ASSOCIATED(T1) ) STOP 25
  IF ( .NOT. ASSOCIATED(T2) ) STOP 26

  IF ( T0%L0       .NE. 3     ) STOP 31
  IF ( SIZE(T0)    .NE. n ) STOP 32



  SELECT TYPE ( T1 )
  TYPE IS (DT2(1,*,4,*,8,*))
  DO I=1, N
    IF ( T1%L0        .NE. 3         )  STOP 60
    IF ( T1%L1        .NE. 5         )  STOP 61
    IF ( SIZE(T1)     .NE. N         )  STOP 62
    IF ( ANY( T1%C1   .NE. "XYZ"   ) )  STOP 63

    IF ( T1(I)%C1             .NE.   "XYZ"    )  STOP 64
    IF ( T1(I)%C2             .NE.   "ZYX"    )  STOP 65
    IF ( T1(I)%I              .NE.   1234     )  STOP 66
    IF ( T1(I)%R              .NE.   4321.    )  STOP 67
    IF ( T1(I)%L              .NEQV. .TRUE.   )  STOP 68
    IF ( T1(I)%Z              .NE.   (1.,-1.) )  STOP 69
    IF ( T1(I)%T0%K0          .NE.    8       )  STOP 60
    IF ( T1(I)%T0%L0          .NE.    7       )  STOP 71
    IF ( ASSOCIATED(T1(I)%Ptr).EQV.   .TRUE.  )  STOP 72
    IF ( SIZE(T1(I)%T0)       .ne.    L2      )  STOP 73
    IF ( T1(I)%T0%K0          .NE.    8       )  STOP 74
    IF ( T1(I)%T0%L0          .NE.    7       )  STOP 75
  END DO


  CLASS DEFAULT
    STOP 99
  END SELECT


  IF ( T2%L0        .NE. 3         )  STOP 80
  IF ( T2%L1        .NE. 5         )  STOP 81
  IF ( T2%L2        .NE. 7         )  STOP 82
  IF ( SIZE(T2)     .NE. N         )  STOP 83

  DO I=1, N
    IF ( T2(I)%C1             .NE.   "XYZ"    )  STOP 84
    IF ( T2(I)%C2             .NE.   "ZYX"    )  STOP 85
    IF ( T2(I)%I              .NE.   1234     )  STOP 86
    IF ( T2(I)%R              .NE.   4321.    )  STOP 87
    IF ( T2(I)%L              .NEQV. .TRUE.   )  STOP 88
    IF ( T2(I)%Z              .NE.   (1.,-1.) )  STOP 89
    IF ( T2(I)%T0%K0          .NE.    8       )  STOP 90
    IF ( T2(I)%T0%L0          .NE.    7       )  STOP 91
    IF ( ASSOCIATED(T2(I)%Ptr).EQV.   .TRUE.  )  STOP 92
    IF ( SIZE(T2(I)%T0)       .ne.    L2      )  STOP 93
    IF ( T2(I)%T0%K0          .NE.    8       )  STOP 94
    IF ( T2(I)%T0%L0          .NE.    7       )  STOP 95
  END DO

  END


