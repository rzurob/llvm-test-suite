!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr26
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 04, 2007
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
!*  -- SAVE 
!*  
!*
!* 
!*  (337560)
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE Mod

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND    :: K1=K0
    INTEGER(K0), LEN     :: L1=K0
    CHARACTER(L1+3) :: C1 = "DT1" 
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
    TYPE(DT0(K2, L2))           :: T0 
    TYPE(DT2(K0,L0,K1,L1,K2, L2)), POINTER  :: Ptr
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun0
  END TYPE

  TYPE, EXTENDS(DT2) :: DT
    TYPE(DT0(1,1)),         ALLOCATABLE  :: Tar0(:)
    TYPE(DT0(1,:)),         POINTER      :: Ptr0(:)=>NULL()

    CLASS(DT1(1,:, 4,:)),   POINTER      :: Tar1(:)
    CLASS(DT1(1,:, 4,:)),   POINTER      :: Ptr1(:)=>NULL()

    TYPE(Dt2(1,3,4,5,8,7)), ALLOCATABLE  :: Tar2(:)
    TYPE(Dt2(1,3,4,5,8,7)), POINTER      :: Ptr2(:)=>NULL()
  END TYPE

  CONTAINS

  FUNCTION ModFun0(Arg)
  CLASS(DT0(1,1)), TARGET, INTENT(IN)  :: Arg(:) 
  CLASS(DT0(1,1)), POINTER             :: ModFun0(:) 
    ModFun0 => Arg 
  END FUNCTION 

  END MODULE

  SUBROUTINE ExtSub(M, N)
  USE Mod
  IMPLICIT NONE

  INTEGER M, N, I

  TYPE(DT(1,3,4,5,8,7)), SAVE, TARGET :: T

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

  SELECT CASE (M)
  CASE (1)
    IF (  ALLOCATED ( T%Tar0) ) STOP 11
    IF (  ASSOCIATED( T%Tar1) ) STOP 12
    IF (  ALLOCATED ( T%Tar2) ) STOP 13

    IF (  ASSOCIATED(T%Ptr0) ) STOP 14
    IF (  ASSOCIATED(T%Ptr1) ) STOP 15
    IF (  ASSOCIATED(T%Ptr2) ) STOP 16

    ALLOCATE(T%Tar0(N))
    ALLOCATE(T%Tar1(N), SOURCE=CT)
    ALLOCATE(T%Tar2(N), SOURCE=CT)

    T%Ptr0(N:) => T%Tar0
    T%Ptr1(N:) => T%Tar1
    T%Ptr2(N:) => T%Tar2
    
  CASE (2)
    IF ( .NOT. ALLOCATED ( T%Tar0) ) STOP 21
    IF ( .NOT. ASSOCIATED( T%Tar1) ) STOP 22
    IF ( .NOT. ALLOCATED ( T%Tar2) ) STOP 23

    IF ( .NOT. ASSOCIATED(T%Ptr0) ) STOP 24
    IF ( .NOT. ASSOCIATED(T%Ptr1) ) STOP 25
    IF ( .NOT. ASSOCIATED(T%Ptr2) ) STOP 26

    IF ( T%Ptr0%L0         .NE. 1 ) STOP 31
    IF ( LBOUND(T%Ptr0,1)  .NE. N ) STOP 32
    IF ( SIZE(T%Ptr0)      .NE. N ) STOP 33

    IF ( T%Ptr1%L0        .NE. 3       )  STOP 41
    IF ( T%Ptr1%L1        .NE. 5       )  STOP 42
    IF ( LBOUND(T%Ptr1, 1).NE. N       )  STOP 43
    IF ( SIZE(T%Ptr1)     .NE. N       )  STOP 44
    IF ( ANY( T%Ptr1%C1   .NE. "XYZ" ) )  STOP 45

    IF ( T%Ptr2%L0        .NE. 3       )  STOP 41
    IF ( T%Ptr2%L1        .NE. 5       )  STOP 42
    IF ( T%Ptr2%L2        .NE. 7       )  STOP 43
    IF ( LBOUND(T%ptr2,1) .NE. N       )  STOP 44
    IF ( SIZE(T%Ptr2)     .NE. N       )  STOP 45

    DO I=N, 2*N-1
      IF ( T%Ptr2(I)%L0                .NE.   3        )  STOP 81
      IF ( T%Ptr2(I)%L1                .NE.   5        )  STOP 82
      IF ( T%Ptr2(I)%L2                 .NE.   7       )  STOP 83
      IF ( T%Ptr2(I)%C1                .NE.   "XYZ"    )  STOP 84
      IF ( T%Ptr2(I)%C2                .NE.   "ZYX"    )  STOP 85
      IF ( T%Ptr2(I)%I                 .NE.   1234     )  STOP 86
      IF ( T%Ptr2(I)%R                 .NE.   4321.    )  STOP 87
      IF ( T%Ptr2(I)%L                 .NEQV. .TRUE.   )  STOP 88
      IF ( T%Ptr2(I)%Z                 .NE.   (1.,-1.) )  STOP 89
      IF ( T%Ptr2(I)%T0%K0             .NE.    8       )  STOP 90
      IF ( T%Ptr2(I)%T0%L0             .NE.    7       )  STOP 91
      IF ( ASSOCIATED( T%Ptr2(I)%Ptr ) .EQV.   .TRUE.  )  STOP 92
      IF ( T%Ptr2(I)%Ptr%K2            .NE.    8       )  STOP 93
      IF ( T%Ptr2(I)%Ptr%L2            .NE.    7       )  STOP 94
    END DO

  CASE DEFAULT
    STOP 10
  END SELECT

  END SUBROUTINE


  PROGRAM dtpObjDecAttr26
  IMPLICIT NONE

  INTEGER :: N = 113

  CALL ExtSub(1, N)

  CALL ExtSub(2, N)

  END


