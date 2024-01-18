!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr25
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
!*  (337546)
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
    TYPE(DT2(K0,L0,K1,L1,K2, L2)), POINTER  :: Ptr2
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun0
  END TYPE

  CONTAINS

  FUNCTION ModFun0(Arg)
  CLASS(DT0(1,1)), TARGET, INTENT(IN)  :: Arg(:) 
  CLASS(DT0(1,1)), POINTER             :: ModFun0(:) 
    ModFun0 => Arg 
  END FUNCTION 


  END MODULE

  PROGRAM dtpObjDecAttr25
  USE M
  IMPLICIT NONE

  INTEGER :: N = 113

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
  CALL IntSub(1, N)

  CALL IntSub(2, N)

  CONTAINS

  SUBROUTINE IntSub(M, N)
  INTEGER M, N, I

  TYPE(DT0(1,1)),         Target, ALLOCATABLE,  SAVE  :: Tar0(:)
  TYPE(DT0(1,:)),                 POINTER,      SAVE  :: Ptr0(:)=>NULL()

  CLASS(DT1(1,:, 4,:)),           POINTER,      SAVE  :: Tar1(:)
  CLASS(DT1(1,:, 4,:)),           POINTER  ,    SAVE  :: Ptr1(:)=>NULL()

  TYPE(Dt2(1,3,4,5,8,7)), TARGET, ALLOCATABLE , SAVE  :: Tar2(:)
  TYPE(Dt2(1,3,4,5,8,7)),         POINTER   ,   SAVE  :: Ptr2(:)=>NULL()

  SELECT CASE (M)
  CASE (1)
    IF (  ALLOCATED ( Tar0) ) STOP 11
    IF (  ASSOCIATED( Tar1) ) STOP 12
    IF (  ALLOCATED ( Tar2) ) STOP 13

    IF (  ASSOCIATED(Ptr0) ) STOP 14
    IF (  ASSOCIATED(Ptr1) ) STOP 15
    IF (  ASSOCIATED(Ptr2) ) STOP 16

    ALLOCATE(Tar0(N))
    ALLOCATE(Tar1(N), SOURCE=T)
    ALLOCATE(Tar2(N), SOURCE=T)

    Ptr0(N:) => Tar0
    Ptr1(N:) => Tar1
    Ptr2(N:) => Tar2
    
  CASE (2)
    IF ( .NOT. ALLOCATED ( Tar0) ) STOP 21
    IF ( .NOT. ASSOCIATED( Tar1) ) STOP 22
    IF ( .NOT. ALLOCATED ( Tar2) ) STOP 23

    IF ( .NOT. ASSOCIATED(Ptr0) ) STOP 24
    IF ( .NOT. ASSOCIATED(Ptr1) ) STOP 25
    IF ( .NOT. ASSOCIATED(Ptr2) ) STOP 26

    IF ( Ptr0%L0         .NE. 1 ) STOP 31
    IF ( LBOUND(Ptr0,1)  .NE. N ) STOP 32
    IF ( SIZE(Ptr0)      .NE. N ) STOP 33

    IF ( Ptr1%L0        .NE. 3       )  STOP 41
    IF ( Ptr1%L1        .NE. 5       )  STOP 42
    IF ( LBOUND(ptr1, 1).NE. N       )  STOP 43
    IF ( SIZE(Ptr1)     .NE. N       )  STOP 44
    IF ( ANY( Ptr1%C1   .NE. "XYZ" ) )  STOP 45

    IF ( Ptr2%L0        .NE. 3       )  STOP 41
    IF ( Ptr2%L1        .NE. 5       )  STOP 42
    IF ( Ptr2%L2        .NE. 7       )  STOP 43
    IF ( LBOUND(ptr2,1) .NE. N       )  STOP 44
    IF ( SIZE(Ptr2)     .NE. N       )  STOP 45

    DO I=N, 2*N-1
      IF ( Ptr2(I)%L0                .NE.   3        )  STOP 81
      IF ( Ptr2(I)%L1                .NE.   5        )  STOP 82
      IF ( Ptr2(I)%L2                 .NE.   7       )  STOP 83
      IF ( Ptr2(I)%C1                .NE.   "XYZ"    )  STOP 84
      IF ( Ptr2(I)%C2                .NE.   "ZYX"    )  STOP 85
      IF ( Ptr2(I)%I                 .NE.   1234     )  STOP 86
      IF ( Ptr2(I)%R                 .NE.   4321.    )  STOP 87
      IF ( Ptr2(I)%L                 .NEQV. .TRUE.   )  STOP 88
      IF ( Ptr2(I)%Z                 .NE.   (1.,-1.) )  STOP 89
      IF ( Ptr2(I)%T0%K0             .NE.    8       )  STOP 90
      IF ( Ptr2(I)%T0%L0             .NE.    7       )  STOP 91
      IF ( ASSOCIATED( Ptr2(I)%Ptr2) .EQV.   .TRUE.  )  STOP 92
      IF ( Ptr2(I)%Ptr2%K2           .NE.    8       )  STOP 93
      IF ( Ptr2(I)%Ptr2%L2           .NE.    7       )  STOP 94
    END DO

  CASE DEFAULT
    STOP 10
  END SELECT

  END SUBROUTINE

  END


