!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr29
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
!*  (337627)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=3
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND    :: K1=K0
    INTEGER(K0), LEN     :: L1=5
    CHARACTER(L1+3)      :: C1 = "DT1"
    CONTAINS
    PROCEDURE(ModFun0), NOPASS, DEFERRED :: Proc
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND    :: K2=K1
    INTEGER(K1), LEN     :: L2=7
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

  TYPE, EXTENDS(DT2) :: DT
    TYPE(DT0(1,1))                      :: V0(L0)
    CLASS(DT1(1,:, 4,:)),  ALLOCATABLE  :: V1(:)
    TYPE(Dt2(1,3,4,5,8,7))              :: V2(L2)
  END TYPE


  CONTAINS

  FUNCTION ModFun0(Arg)
  CLASS(DT0(1,1)), TARGET, INTENT(IN)  :: Arg(:)
  CLASS(DT0(1,1)), POINTER             :: ModFun0(:)
    ModFun0 => Arg
  END FUNCTION

  SUBROUTINE ModSub(L0, L1, L2, S, Status)
  IMPLICIT NONE

  INTEGER L0, Status, I
  INTEGER(1) L1
  INTEGER(4) L2

  !TYPE(DT(1,L0,4,L1,8,L2)), VALUE :: S   ! --> C528
  TYPE(DT(K0=1,K1=4,K2=8)), VALUE :: S   ! --> C528

  IF ( Status .EQ. 1 ) THEN
    IF ( ALLOCATED (S%V1) ) STOP 15
  ELSE

    IF ( .NOT. ALLOCATED (S%V1)      )  STOP 25
    IF ( S%V0%L0         .NE. 1     )  STOP 31
    IF ( SIZE(S%V0)      .NE. L0     )  STOP 32

    SELECT TYPE ( As =>S%V1)
    TYPE IS (DT2(1,*,4,*,8,*))

      IF ( AS%L0        .NE. L0      )  STOP 41
      IF ( As%L1        .NE. L1      )  STOP 42
      IF ( As%L2        .NE. L2      )  STOP 42
      IF ( SIZE(As)     .NE. L1      )  STOP 43

      DO I=1, As(1)%L1
        IF ( As(I)%C1                .NE.   "XYZ"    )  STOP 44
        IF ( As(I)%C2                .NE.   "ZYX"    )  STOP 45
        IF ( As(I)%I                 .NE.   1234     )  STOP 46
        IF ( As(I)%R                 .NE.   4321.    )  STOP 47
        IF ( As(I)%L                 .NEQV. .TRUE.   )  STOP 48
        IF ( As(I)%Z                 .NE.   (1.,-1.) )  STOP 49
        IF ( As(I)%T0%K0             .NE.    8       )  STOP 50
        IF ( As(I)%T0%L0             .NE.    7       )  STOP 51
        IF ( ASSOCIATED(As(I)%Ptr )  .EQV.   .TRUE.  )  STOP 52
        IF ( As(I)%Ptr%K2            .NE.    8       )  STOP 53
        IF ( As(I)%Ptr%L2            .NE.    7       )  STOP 54
      END DO
    CLASS DEFAULT
       STOP 99
    END SELECT

    IF ( S%V2%L0        .NE. L0      )  STOP 61
    IF ( S%V2%L1        .NE. L1      )  STOP 62
    IF ( S%V2%L2        .NE. L2      )  STOP 62
    IF ( SIZE(S%V2)     .NE. L2      )  STOP 63

    DO I=1, L2
      IF ( S%V2(I)%C1                .NE.   "XYZ"    )  STOP 64
      IF ( S%V2(I)%C2                .NE.   "ZYX"    )  STOP 65
      IF ( S%V2(I)%I                 .NE.   1234     )  STOP 66
      IF ( S%V2(I)%R                 .NE.   4321.    )  STOP 67
      IF ( S%V2(I)%L                 .NEQV. .TRUE.   )  STOP 68
      IF ( S%V2(I)%Z                 .NE.   (1.,-1.) )  STOP 69
      IF ( S%V2(I)%T0%K0             .NE.    8       )  STOP 70
      IF ( S%V2(I)%T0%L0             .NE.    7       )  STOP 71
      IF ( ASSOCIATED(S%V2(I)%Ptr )  .EQV.   .TRUE.  )  STOP 72
      IF ( S%V2(I)%Ptr%K2            .NE.    8       )  STOP 73
      IF ( S%V2(I)%Ptr%L2            .NE.    7       )  STOP 74
    END DO
  END IF

  END SUBROUTINE

  END MODULE


  PROGRAM dtpObjDecAttr29
  USE M
  IMPLICIT NONE
  INTEGER I

  TYPE(DT(1,3,4,5,8,7)) :: T

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

  CALL ModSub(T%L0, T%L1, T%L2, T, 1)

  T = DT(1,3,4,5,8,7)(T0=DT0(8,7)(), ptr = null(), V0=DT0(1,1)(), V1=[(CT,I=1, T%L1)], V2=CT)
  CALL ModSub(T%L0, T%L1, T%L2, T, 2)

  END


