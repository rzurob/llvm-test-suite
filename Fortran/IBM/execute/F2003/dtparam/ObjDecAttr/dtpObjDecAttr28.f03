!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 05, 2007
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
!*  (complain on line 103)
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
    TYPE(Dt2(1,3,4,5,8,7))              :: Tar2(L2)
  END TYPE

  INTEGER, PARAMETER             :: N = 113
  TYPE(DT(1,3,4,5,8,7)), POINTER :: T(:)

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

  SUBROUTINE ModSub()
  IMPLICIT NONE

  ALLOCATE(T(N), SOURCE=DT(1,3,4,5,8,7)(T0=DT0(8,7)(), ptr = null(), Tar0=DT0(1,1)(), Tar2=CT))
  Ptr0 => T(N)%Tar0
  Ptr2 => T(N)%Tar2

  END SUBROUTINE

  END MODULE


  PROGRAM dtpObjDecAttr28
  USE M
  IMPLICIT NONE
  INTEGER I

  CALL ModSub()

  IF ( .NOT. ASSOCIATED(Ptr0) ) ERROR STOP 24
  IF ( .NOT. ASSOCIATED(Ptr2) ) ERROR STOP 26

  IF ( Ptr0%L0         .NE. 1 )            ERROR STOP 31
  IF ( SIZE(Ptr0)      .NE. T%L0 ) ERROR STOP 32

  IF ( Ptr2%L0        .NE. 3            )  ERROR STOP 51
  IF ( Ptr2%L1        .NE. 5            )  ERROR STOP 52
  IF ( Ptr2%L2        .NE. 7            )  ERROR STOP 53
  IF ( SIZE(Ptr2)     .NE. T%Tar2(1)%L2 )  ERROR STOP 54

  DO I=1, T(1)%Tar2%L2
    IF ( Ptr2(I)%C1                .NE.   "XYZ"    )  ERROR STOP 84
    IF ( Ptr2(I)%C2                .NE.   "ZYX"    )  ERROR STOP 85
    IF ( Ptr2(I)%I                 .NE.   1234     )  ERROR STOP 86
    IF ( Ptr2(I)%R                 .NE.   4321.    )  ERROR STOP 87
    IF ( Ptr2(I)%L                 .NEQV. .TRUE.   )  ERROR STOP 88
    IF ( Ptr2(I)%Z                 .NE.   (1.,-1.) )  ERROR STOP 89
    IF ( Ptr2(I)%T0%K0             .NE.    8       )  ERROR STOP 90
    IF ( Ptr2(I)%T0%L0             .NE.    7       )  ERROR STOP 91
    IF ( ASSOCIATED(Ptr2(I)%Ptr )  .EQV.   .TRUE.  )  ERROR STOP 92
    IF ( Ptr2(I)%Ptr%K2            .NE.    8       )  ERROR STOP 93
    IF ( Ptr2(I)%Ptr%L2            .NE.    7       )  ERROR STOP 94
  END DO

  END

