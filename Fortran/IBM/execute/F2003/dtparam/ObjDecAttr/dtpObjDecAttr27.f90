!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 04, 2007
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
!*  -- TARGET
!*
!*  (ICE-spec expr)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

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
    TYPE(DT0(K2, L2))    :: T0
    TYPE(DT2(K0,L0,K1,L1,K2, L2)), POINTER  :: Ptr
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun0
  END TYPE

  TYPE, EXTENDS(DT2) :: DT
    TYPE(DT0(1,1))                      :: Tar0(L0)
    CLASS(DT1(1,:, 4,:)),  ALLOCATABLE  :: Tar1(:)
    TYPE(Dt2(1,3,4,5,8,7))              :: Tar2(L2)
  END TYPE

  INTEGER, PARAMETER                  :: N = 113
  TYPE(DT(1,3,4,5,8,7)), TARGET, SAVE :: T(N)

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

  TYPE(DT0(1,:)),         POINTER      :: Ptr0(:)
  CLASS(DT1(1,:, 4,:)),   POINTER      :: Ptr1(:)
  TYPE(DT2(1,3,4,5,8,7)), POINTER      :: Ptr2(:)

  CONTAINS

  FUNCTION ModFun0(Arg)
  CLASS(DT0(1,1)), TARGET, INTENT(IN)  :: Arg(:)
  CLASS(DT0(1,1)), POINTER             :: ModFun0(:)
    ModFun0 => Arg
  END FUNCTION

  SUBROUTINE ModSub(L0, L1, L2, N, S)
  IMPLICIT NONE

  INTEGER L0
  INTEGER(T%K0) L1
  INTEGER(T%K1) L2
  INTEGER N

  TYPE(DT(1,L0,4,L1,8,L2)), TARGET :: S(N)

  ALLOCATE(S(N)%Tar1(L1), SOURCE=CT)

  Ptr0 => S(N)%Tar0
  Ptr1 => S(N)%Tar1
  Ptr2 => S(N)%Tar2

  Ptr2 = CT

  END SUBROUTINE

  END MODULE


  PROGRAM dtpObjDecAttr27
  USE M
  IMPLICIT NONE
  INTEGER I

  CALL ModSub(T%L0, T%L1, T%L2, N, T)

  IF ( .NOT. ASSOCIATED(Ptr0) ) STOP 24
  IF ( .NOT. ASSOCIATED(Ptr1) ) STOP 25
  IF ( .NOT. ASSOCIATED(Ptr2) ) STOP 26

  IF ( Ptr0%L0         .NE. 1 )            STOP 31
  IF ( SIZE(Ptr0)      .NE. T%L0 ) STOP 32


  IF ( Ptr1%L0        .NE. 3            )  STOP 41
  IF ( Ptr1%L1        .NE. 5            )  STOP 42
  IF ( SIZE(Ptr1)     .NE. T(1)%L1 )  STOP 43
  IF ( ANY( Ptr1%C1   .NE. "XYZ"      ) )  STOP 44

  IF ( Ptr2%L0        .NE. 3            )  STOP 51
  IF ( Ptr2%L1        .NE. 5            )  STOP 52
  IF ( Ptr2%L2        .NE. 7            )  STOP 53
  IF ( SIZE(Ptr2)     .NE. T%Tar2(1)%L2 )  STOP 54

  DO I=1, T(1)%Tar2%L2
    IF ( Ptr2(I)%C1                .NE.   "XYZ"    )  STOP 84
    IF ( Ptr2(I)%C2                .NE.   "ZYX"    )  STOP 85
    IF ( Ptr2(I)%I                 .NE.   1234     )  STOP 86
    IF ( Ptr2(I)%R                 .NE.   4321.    )  STOP 87
    IF ( Ptr2(I)%L                 .NEQV. .TRUE.   )  STOP 88
    IF ( Ptr2(I)%Z                 .NE.   (1.,-1.) )  STOP 89
    IF ( Ptr2(I)%T0%K0             .NE.    8       )  STOP 90
    IF ( Ptr2(I)%T0%L0             .NE.    7       )  STOP 91
    IF ( ASSOCIATED(Ptr2(I)%Ptr )  .EQV.   .TRUE.  )  STOP 92
    IF ( Ptr2(I)%Ptr%K2            .NE.    8       )  STOP 93
    IF ( Ptr2(I)%Ptr%L2            .NE.    7       )  STOP 94
  END DO

  END


