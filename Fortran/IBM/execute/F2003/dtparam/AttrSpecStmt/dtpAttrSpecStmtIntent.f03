!*********************************************************************
!*  ===================================================================
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
!*  -- INTENT statement
!*
!*  (ice)
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
    TYPE(DT0(K2, L2))    :: T0(L2)
    TYPE(DT2(K0,L0,K1,L1,K2, L2)), POINTER  :: Ptr
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun0
  END TYPE

  DIMENSION              :: T0(:), T2(:)
  DIMENSION              :: T1(:)
  POINTER                :: T0, T2
  POINTER                :: T1

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

  SUBROUTINE ExtSub(T0, T1, T2)
  USE M, ONLY : DT0, DT1, DT2

  INTENT(OUT)            :: T0
  INTENT(IN)             :: T1
  INTENT(INOUT)          :: T2

  DIMENSION              :: T0(:), T2(:)
  DIMENSION              :: T1(:)
  POINTER                :: T0, T2
  POINTER                :: T1

  TYPE(DT0(1,3))         :: T0
  TYPE(Dt2(1,3,4,5,8,7)) :: T2
  CLASS(DT1(1,:, 4,:))   :: T1

  integer :: n = 64

  IF ( .NOT. ASSOCIATED(T0) ) ERROR STOP 24
  IF ( .NOT. ASSOCIATED(T1) ) ERROR STOP 25
  IF ( .NOT. ASSOCIATED(T2) ) ERROR STOP 26

  IF ( T0%L0       .NE. 3     ) ERROR STOP 31
  IF ( SIZE(T0)    .NE. N ) ERROR STOP 32

  SELECT TYPE ( T1 )
  TYPE IS (DT2(1,*,4,*,8,*))
  DO I=1, N
    IF ( T1%L0        .NE. 3         )  ERROR STOP 60
    IF ( T1%L1        .NE. 5         )  ERROR STOP 61
    IF ( SIZE(T1)     .NE. N         )  ERROR STOP 62
    IF ( ANY( T1%C1   .NE. "XYZ"   ) )  ERROR STOP 63

    IF ( T1(I)%C1             .NE.   "XYZ"    )  ERROR STOP 64
    IF ( T1(I)%C2             .NE.   "ZYX"    )  ERROR STOP 65
    IF ( T1(I)%I              .NE.   1234     )  ERROR STOP 66
    IF ( T1(I)%R              .NE.   4321.    )  ERROR STOP 67
    IF ( T1(I)%L              .NEQV. .TRUE.   )  ERROR STOP 68
    IF ( T1(I)%Z              .NE.   (1.,-1.) )  ERROR STOP 69
    IF ( T1(I)%T0%K0          .NE.    8       )  ERROR STOP 60
    IF ( T1(I)%T0%L0          .NE.    7       )  ERROR STOP 71
    IF ( ASSOCIATED(T1(I)%Ptr).EQV.   .TRUE.  )  ERROR STOP 72
    IF ( SIZE(T1(I)%T0)       .EQ.    L2      )  ERROR STOP 73
    IF ( T1(I)%T0%K0          .NE.    8       )  ERROR STOP 74
    IF ( T1(I)%T0%L0          .NE.    7       )  ERROR STOP 75
  END DO

  CLASS DEFAULT
    STOP 99
  END SELECT


  IF ( T2%L0        .NE. 3         )  ERROR STOP 80
  IF ( T2%L1        .NE. 5         )  ERROR STOP 81
  IF ( T2%L2        .NE. 7         )  ERROR STOP 82
  IF ( SIZE(T2)     .NE. N         )  ERROR STOP 83

  DO I=1, N
    IF ( T2(I)%C1             .NE.   "XYZ"    )  ERROR STOP 84
    IF ( T2(I)%C2             .NE.   "ZYX"    )  ERROR STOP 85
    IF ( T2(I)%I              .NE.   1234     )  ERROR STOP 86
    IF ( T2(I)%R              .NE.   4321.    )  ERROR STOP 87
    IF ( T2(I)%L              .NEQV. .TRUE.   )  ERROR STOP 88
    IF ( T2(I)%Z              .NE.   (1.,-1.) )  ERROR STOP 89
    IF ( T2(I)%T0%K0          .NE.    8       )  ERROR STOP 90
    IF ( T2(I)%T0%L0          .NE.    7       )  ERROR STOP 91
    IF ( ASSOCIATED(T2(I)%Ptr).EQV.   .TRUE.  )  ERROR STOP 92
    IF ( SIZE(T2(I)%T0)       .EQ.    L2      )  ERROR STOP 93
    IF ( T2(I)%T0%K0          .NE.    8       )  ERROR STOP 94
    IF ( T2(I)%T0%L0          .NE.    7       )  ERROR STOP 95
  END DO

  END SUBROUTINE


  PROGRAM dtpAttrSpecStmtIntent
  USE M
  IMPLICIT NONE
  INTEGER I

  INTEGER L0
  INTEGER L1
  INTEGER L2
  INTEGER N

  interface
    subroutine extsub (t0, t1, t2)
    use m, only: dt0, dt1, dt2
        intent(out) t0
        intent(in) t1
        intent(inout) t2
        dimension t0(:), t2(:), t1(:)
        pointer t0, t2, t1

        type(dt0(1,3)) t0
        type(dt2(1,3,4,5,8,7)) t2
        class(dt1(1,:,4,:)) t1
    end subroutine
  end interface

  L0 = 3; L1 = 5; L2 = 7; N = 64
  CALL ModSub(L0, L1, L2, N)

  CALL ExtSub( T0, T1, T2 )

  END

