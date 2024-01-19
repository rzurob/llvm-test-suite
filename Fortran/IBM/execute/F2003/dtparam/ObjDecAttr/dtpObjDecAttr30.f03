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
!*  -- TARGET / Polymorphic dummy
!*
!*  ()
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
    TYPE(DT2(K0,L0,K1,L1,K2, L2)), POINTER  :: Ptr => null()
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun0
  END TYPE

  TYPE, EXTENDS(DT2) :: DT
    TYPE(DT0(1,1)),        POINTER      :: V0(:)
    CLASS(DT1(1,:, 4,:)),  ALLOCATABLE  :: V1(:)
    TYPE(Dt2(1,3,4,5,8,7)),POINTER      :: V2(:)
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
  INTEGER(4) L1
  INTEGER(8) L2

  CLASS(DT(1,*,4,*,8)), intent(in) :: S

  IF ( Status .EQ. 1 ) THEN
    IF ( ASSOCIATED(S%V0) ) ERROR STOP 14
    IF ( ALLOCATED (S%V1) ) ERROR STOP 15
    IF ( ASSOCIATED(S%V2) ) ERROR STOP 16
  ELSE

    SELECT TYPE ( S )
    CLASS IS (DT(1,*,4,*,8,*))

    IF ( .NOT. ASSOCIATED(S%V0) ) ERROR STOP 24
    IF ( .NOT. ALLOCATED (S%V1) ) ERROR STOP 25
    IF ( .NOT. ASSOCIATED(S%V2) ) ERROR STOP 26

    IF ( S%V0%L0         .NE. L0     )  ERROR STOP 31
    IF ( SIZE(S%V0)      .NE. L0     )  ERROR STOP 32

    SELECT TYPE ( As =>S%V1)
    TYPE IS (DT2(1,*,4,*,8,*))

      IF ( AS%L0        .NE. L0      )  ERROR STOP 41
      IF ( As%L1        .NE. L1      )  ERROR STOP 42
      IF ( As%L2        .NE. L2      )  ERROR STOP 42
      IF ( SIZE(As)     .NE. L2      )  ERROR STOP 43

      DO I=1, As(1)%L1
        IF ( As(I)%C1                .NE.   "XYZ"    )  ERROR STOP 44
        IF ( As(I)%C2                .NE.   "ZYX"    )  ERROR STOP 45
        IF ( As(I)%I                 .NE.   1234     )  ERROR STOP 46
        IF ( As(I)%R                 .NE.   4321.    )  ERROR STOP 47
        IF ( As(I)%L                 .NEQV. .TRUE.   )  ERROR STOP 48
        IF ( As(I)%Z                 .NE.   (1.,-1.) )  ERROR STOP 49
        IF ( As(I)%T0%K0             .NE.    8       )  ERROR STOP 50
        IF ( As(I)%T0%L0             .NE.    7       )  ERROR STOP 51
        IF ( ASSOCIATED(As(I)%Ptr )  .EQV.   .TRUE.  )  ERROR STOP 52
        IF ( As(I)%Ptr%K2            .NE.    8       )  ERROR STOP 53
        IF ( As(I)%Ptr%L2            .NE.    7       )  ERROR STOP 54
      END DO
    CLASS DEFAULT
       STOP 99
    END SELECT

    IF ( S%V2%L0        .NE. L0      )  ERROR STOP 61
    IF ( S%V2%L1        .NE. L1      )  ERROR STOP 62
    IF ( S%V2%L2        .NE. L2      )  ERROR STOP 62
    IF ( SIZE(S%V2)     .NE. L2      )  ERROR STOP 63

    DO I=1, L2
      IF ( S%V2(I)%C1                .NE.   "XYZ"    )  ERROR STOP 64
      IF ( S%V2(I)%C2                .NE.   "ZYX"    )  ERROR STOP 65
      IF ( S%V2(I)%I                 .NE.   1234     )  ERROR STOP 66
      IF ( S%V2(I)%R                 .NE.   4321.    )  ERROR STOP 67
      IF ( S%V2(I)%L                 .NEQV. .TRUE.   )  ERROR STOP 68
      IF ( S%V2(I)%Z                 .NE.   (1.,-1.) )  ERROR STOP 69
      IF ( S%V2(I)%T0%K0             .NE.    8       )  ERROR STOP 70
      IF ( S%V2(I)%T0%L0             .NE.    7       )  ERROR STOP 71
      IF ( ASSOCIATED(S%V2(I)%Ptr )  .EQV.   .TRUE.  )  ERROR STOP 72
      IF ( S%V2(I)%Ptr%K2            .NE.    8       )  ERROR STOP 73
      IF ( S%V2(I)%Ptr%L2            .NE.    7       )  ERROR STOP 74
    END DO

  CLASS DEFAULT
    STOP 80
  END SELECT

  END IF

  END SUBROUTINE

  END MODULE


  PROGRAM dtpObjDecAttr30
  USE M
  IMPLICIT NONE
  INTEGER I

  CLASS(DT(1,3,4,5,8,7)), ALLOCATABLE :: T

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

  ALLOCATE(T, SOURCE = DT(1,3,4,5,8,7)(T0=DT0(8,7)(), V0=NULL(), V1=NULL(), V2=NULL()))
  CALL ModSub(3,5,7_8,T, 1)

  deallocate (t)
!  allocate(t)
!  CALL ModSub(3,5,7_8,T, 2)

  END


