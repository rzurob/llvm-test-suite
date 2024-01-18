!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr21
!*
!*  DATE                       : Jun. 01, 2007
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
!*  POINTER -- dummy
!*
!*  (337497)
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
    CHARACTER(L1+3) :: C1 = "DT1"
    CONTAINS
    PROCEDURE(ModSub), NOPASS, DEFERRED :: Proc
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND    :: K2=K1
    INTEGER(K1), LEN     :: L2=K1
    CHARACTER(L2)        :: C2=CHAR(K2)
    INTEGER(K2)          :: I=K2
    REAL   (K2)          :: R=K2
    LOGICAL(K2)          :: L=.TRUE._1
    COMPLEX(K2)          :: Z=CMPLX(K1, K2, K2)
    TYPE(DT0(K2, L2))           :: T0
    TYPE(DT2(K0,L0,K1,L1,K2,L2)), POINTER  :: Ptr2
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModSub
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Obj,Arg)
  CLASS(DT2(1,*,4,*,8,*)) :: Obj
  TYPE (DT2(1,*,4,*,8,*)) :: Arg
    Arg = Obj
  END SUBROUTINE

  END MODULE

  PROGRAM dtpObjDecAttr21
  USE M

  TYPE(DT0(1,2)),  PARAMETER :: T1=DT0(1,2)()

  TYPE(DT2(1,3,4,5,8,7)), PARAMETER ::             &
        T =  DT2(1,3,4,5,8,7)   (                  &
                                  C1 = "XYZ",      &
                                  C2 = "ZYX",      &
                                   I = 1234,       &
                                   R = 4321.,      &
                                   L = .TRUE.,     &
                                   Z = (1.,-1.),   &
                                   Ptr2 = NULL(),  &
                                   T0=DT0(8,7)() )

  TYPE(DT0(1,1)),         Target               :: Tar0
  CLASS(DT1(1,:, 4,:)),   Target, ALLOCATABLE  :: Tar1
  TYPE(Dt2(1,3,4,5,8,7)), TARGET               :: Tar2

  TYPE(DT0(1,:)),  POINTER :: Ptr01
  CLASS(DT0(1,:)), POINTER :: Ptr02

  CLASS(DT1(1,:,4,:)), POINTER :: Ptr11

  TYPE (DT2(1,:,4,:,8,:)), POINTER :: Ptr21
  CLASS(DT2(1,:,4,:,8,:)), POINTER :: Ptr22

  ALLOCATE(Tar1, SOURCE=T)
  Tar2 = T

  CALL IntSub(Tar0, Tar1, Tar2)

  CONTAINS

  SUBROUTINE IntSub(Tar0, Tar1, Tar2)
  TYPE(DT0(1,1)),         Target :: Tar0
  CLASS(DT1(1,*, 4,*)),   Target :: Tar1
  TYPE(Dt2(1,3,4,5,8,7)), TARGET :: Tar2


  Ptr01 => Tar0
  IF ( Ptr01%L0  .NE. 1 ) STOP 11

  Ptr02 => Tar0
  IF ( Ptr02%L0 .NE. 1 ) STOP 12

  Ptr02 => Tar2
  IF ( Ptr02%L0 .NE. 3 ) STOP 13

  Ptr11 => Tar1
  IF ( Ptr11%L0 .NE. 3 )      STOP 21
  IF ( Ptr11%L1 .NE. 5 )      STOP 22
  IF ( Ptr11%C1 .NE. "XYZ"  ) STOP 23

  Ptr21 => Tar2
  IF ( Ptr21%L0                .NE.   3        )  STOP 31
  IF ( Ptr21%L1                .NE.   5        )  STOP 32
  IF ( Ptr21%L2                .NE.   7        )  STOP 33
  IF ( Ptr21%C1                .NE.   "XYZ"    )  STOP 34
  IF ( Ptr21%C2                .NE.   "ZYX"    )  STOP 35
  IF ( Ptr21%I                 .NE.   1234     )  STOP 36
  IF ( Ptr21%R                 .NE.   4321.    )  STOP 37
  IF ( Ptr21%L                 .NEQV. .TRUE.   )  STOP 38
  IF ( Ptr21%Z                 .NE.   (1.,-1.) )  STOP 39
  IF ( Ptr21%T0%K0             .NE.    8       )  STOP 40
  IF ( Ptr21%T0%L0             .NE.    7       )  STOP 41
  IF ( ASSOCIATED( Ptr21%Ptr2) .EQV.   .TRUE.  )  STOP 42
  IF ( Ptr21%Ptr2%K2           .NE.    8       )  STOP 43
  IF ( Ptr21%Ptr2%L2           .NE.    7       )  STOP 44

  Ptr22 => Tar2
  IF ( Ptr22%L0                .NE.   3        )  STOP 51
  IF ( Ptr22%L1                .NE.   5        )  STOP 52
  IF ( Ptr22%L2                .NE.   7        )  STOP 53
  IF ( Ptr22%C1                .NE.   "XYZ"    )  STOP 54
  IF ( Ptr22%C2                .NE.   "ZYX"    )  STOP 55
  IF ( Ptr22%I                 .NE.   1234     )  STOP 56
  IF ( Ptr22%R                 .NE.   4321.    )  STOP 57
  IF ( Ptr22%L                 .NEQV. .TRUE.   )  STOP 58
  IF ( Ptr22%Z                 .NE.   (1.,-1.) )  STOP 59
  IF ( Ptr22%T0%K0             .NE.    8       )  STOP 60
  IF ( Ptr22%T0%L0             .NE.    7       )  STOP 61
  IF ( ASSOCIATED( Ptr22%Ptr2) .EQV.   .TRUE.  )  STOP 62
  IF ( Ptr22%Ptr2%K2           .NE.    8       )  STOP 63
  IF ( Ptr22%Ptr2%L2           .NE.    7       )  STOP 64

  END SUBROUTINE

  END


