!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr20
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 31, 2007
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
!*  POINTER  -- array
!*  
!*
!* 
!*  (ICE)
!*   
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
    TYPE(DT2(K0,L0,K1,L1,K2, L2)), POINTER  :: Ptr2
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModSub 
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(Obj,Arg)
  CLASS(DT2(1,*,4,*,8,*)), INTENT(IN) :: Obj
  TYPE (DT2(1,*,4,*,8,*)), INTENT(IN) :: Arg
  END SUBROUTINE 

  END MODULE

  PROGRAM dtpObjDecAttr20
  USE M
  IMPLICIT NONE

  INTEGER :: N = 113, I

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

  TYPE(DT0(1,1)),         Target, ALLOCATABLE  :: Tar0(:)
  CLASS(DT1(1,:, 4,:)),           POINTER      :: Tar1(:)
  TYPE(Dt2(1,3,4,5,8,7)), TARGET, ALLOCATABLE  :: Tar2(:)

  TYPE(DT0(1,:)),  POINTER :: Ptr01(:)
  CLASS(DT0(1,:)), POINTER :: Ptr02(:)

  CLASS(DT1(1,:,4,:)), POINTER :: Ptr11(:)

  TYPE (DT2(1,:,4,:,8,:)), POINTER :: Ptr21(:)
  CLASS(DT2(1,:,4,:,8,:)), POINTER :: Ptr22(:)


  ALLOCATE(Tar0(N))
  ALLOCATE(Tar1(N), SOURCE=T)
  ALLOCATE(Tar2(N), SOURCE=T)

  Ptr01(N:) => Tar0
  
  IF ( .not. associated(ptr01) .or. ASSOCIATED(Ptr01, Tar0) ) STOP 11
  IF ( Ptr01%L0         .NE. 1 ) STOP 12
  IF ( LBOUND(Ptr01,1)  .NE. N ) STOP 13
  IF ( SIZE(Ptr01)      .NE. N ) STOP 14

  Ptr02(N:) => Tar0

  IF ( Ptr02%L0         .NE. 1 ) STOP 22
  IF ( LBOUND(Ptr02,1)  .NE. N ) STOP 23
  IF ( SIZE(Ptr02)      .NE. N ) STOP 24

  Ptr02(N:) => Tar2

  IF ( .NOT. ASSOCIATED(Ptr02, Tar2) ) STOP 31
  IF ( Ptr02%L0         .NE. 3 ) STOP 32
  IF ( LBOUND(Ptr02,1)  .NE. N ) STOP 33
  IF ( SIZE(Ptr02)      .NE. N ) STOP 34

  Ptr11(N:) => Tar1

  IF ( .NOT. ASSOCIATED(Ptr11, Tar1) ) STOP 41
  IF ( LBOUND(ptr11,1)  .NE. N )       STOP 42
  IF ( SIZE(Ptr11)      .NE. N )       STOP 43

  DO I=N, 2*N-1
    IF ( Ptr11(I)%L0         .NE. 3 )       STOP 44
    IF ( Ptr11(I)%L1         .NE. 5 )       STOP 45
    IF ( Ptr11(I)%C1         .NE. "XYZ"  )  STOP 46
  END DO

  Ptr21(N:) => Tar2

  IF ( .NOT. ASSOCIATED(Ptr21, Tar2) ) STOP 51
  IF ( LBOUND(ptr21,1)  .NE. N )       STOP 52
  IF ( SIZE(Ptr21)      .NE. N )       STOP 53

  DO I=N, 2*N-1
    IF ( Ptr21(I)%L0                .NE.   3        )  STOP 54
    IF ( Ptr21(I)%L1                .NE.   5        )  STOP 55
    IF ( Ptr21(I)%L2                .NE.   7        )  STOP 56
    IF ( Ptr21(I)%C1                .NE.   "XYZ"    )  STOP 57
    IF ( Ptr21(I)%C2                .NE.   "ZYX"    )  STOP 58
    IF ( Ptr21(I)%I                 .NE.   1234     )  STOP 59
    IF ( Ptr21(I)%R                 .NE.   4321.    )  STOP 61
    IF ( Ptr21(I)%L                 .NEQV. .TRUE.   )  STOP 62
    IF ( Ptr21(I)%Z                 .NE.   (1.,-1.) )  STOP 63
    IF ( Ptr21(I)%T0%K0             .NE.    8       )  STOP 64
    IF ( Ptr21(I)%T0%L0             .NE.    7       )  STOP 65
    IF ( ASSOCIATED( Ptr21(I)%Ptr2) .EQV.   .TRUE.  )  STOP 66
    IF ( Ptr21(I)%Ptr2%K2           .NE.    8       )  STOP 67
    IF ( Ptr21(I)%Ptr2%L2           .NE.    7       )  STOP 68
  END DO

  Ptr22(N:) => Tar2

  iF ( .NOT. ASSOCIATED(Ptr22, Tar2) ) STOP 71
  IF ( LBOUND(ptr22,1)  .NE. N )       STOP 72
  IF ( SIZE(Ptr22)      .NE. N )       STOP 73

  DO I=N, 2*N-1
    IF ( Ptr22(I)%L0                .NE.   3        )  STOP 81
    IF ( Ptr22(I)%L1                .NE.   5        )  STOP 82
    IF ( Ptr22(I)%L2                 .NE.   7       )  STOP 83
    IF ( Ptr22(I)%C1                .NE.   "XYZ"    )  STOP 84
    IF ( Ptr22(I)%C2                .NE.   "ZYX"    )  STOP 85
    IF ( Ptr22(I)%I                 .NE.   1234     )  STOP 86
    IF ( Ptr22(I)%R                 .NE.   4321.    )  STOP 87
    IF ( Ptr22(I)%L                 .NEQV. .TRUE.   )  STOP 88
    IF ( Ptr22(I)%Z                 .NE.   (1.,-1.) )  STOP 89
    IF ( Ptr22(I)%T0%K0             .NE.    8       )  STOP 90
    IF ( Ptr22(I)%T0%L0             .NE.    7       )  STOP 91
    IF ( ASSOCIATED( Ptr22(I)%Ptr2) .EQV.   .TRUE.  )  STOP 92
    IF ( Ptr22(I)%Ptr2%K2           .NE.    8       )  STOP 93
    IF ( Ptr22(I)%Ptr2%L2           .NE.    7       )  STOP 94
  END DO

  END


