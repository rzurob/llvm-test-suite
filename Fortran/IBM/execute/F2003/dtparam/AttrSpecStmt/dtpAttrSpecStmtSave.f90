!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpAttrSpecStmtSave
!*
!*  DATE                       : Jun. 12, 2007
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
!*  -- SAVE statement
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M


  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND    :: K1=K0
    INTEGER(K0), LEN     :: L1=K0
    CHARACTER(L1+3)      :: C1 = "DT1"
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

  INTEGER,   PARAMETER   :: N=1024


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

  END MODULE

  SUBROUTINE ExtSub(Second)
  USE M
  LOGICAL :: Second

  DIMENSION              :: T0(N), T2(N)
  DIMENSION              :: T1(N)

  TYPE(DT0(1,3))         :: T0
  TYPE(DT1(1,3, 4,5))    :: T1
  TYPE(Dt2(1,3,4,5,8,7)) :: T2


  SAVE  ! let's see how save works


  IF ( .NOT. Second ) THEN
    T0 = CT%DT1%DT0
    T1 = CT%DT1
    T2 = CT
  ELSE

  DO I=1, N

    IF ( T0(I)%L0     .NE. 3         )  STOP 30
    IF ( T1(I)%L0     .NE. 3         )  STOP 31
    IF ( T1(I)%L1     .NE. 5         )  STOP 32
    IF ( T2(I)%L0     .NE. 3         )  STOP 33
    IF ( T2(I)%L1     .NE. 5         )  STOP 34
    IF ( T2(I)%L2     .NE. 7         )  STOP 35

    IF ( T1(I)%C1     .NE. "XYZ"     )  STOP 40

    IF ( T2(I)%C1             .NE.   "XYZ"    )  STOP 51
    IF ( T2(I)%C2             .NE.   "ZYX"    )  STOP 52
    IF ( T2(I)%I              .NE.   1234     )  STOP 53
    IF ( T2(I)%R              .NE.   4321.    )  STOP 54
    IF ( T2(I)%L              .NEQV. .TRUE.   )  STOP 55
    IF ( T2(I)%Z              .NE.   (1.,-1.) )  STOP 56
    IF ( T2(I)%T0%K0          .NE.    8       )  STOP 57
    IF ( T2(I)%T0%L0          .NE.    7       )  STOP 61
    IF ( ASSOCIATED(T2(I)%Ptr).EQV.   .TRUE.  )  STOP 62
    IF ( SIZE(T2(I)%T0)       .NE.    7       )  STOP 63
    IF ( T2(I)%T0%K0          .NE.    8       )  STOP 64
    IF ( T2(I)%T0%L0          .NE.    7       )  STOP 65
  END DO

  END IF

  END SUBROUTINE


  PROGRAM dtpAttrSpecStmtSave
  USE M
  IMPLICIT NONE
  INTEGER I

  INTERFACE
    SUBROUTINE ExtSub(L)
    LOGICAL L
    END SUBROUTINE
  END INTERFACE

  CALL ExtSub(.FALSE.)
  CALL ExtSub(.TRUE.)
  CALL ExtSub(.TRUE.)

  END


