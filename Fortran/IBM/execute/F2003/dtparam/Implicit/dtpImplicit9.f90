!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpImplicit9
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 27, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED CLASS PARAMETERS
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
!*  -- The implicit statement
!*   on function result 
!* 
!*  (complain on line 106 -- interface missmatching)
!*   
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    CONTAINS
    PROCEDURE :: ModFun
  END TYPE 

  TYPE,  EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND :: K1=1
    INTEGER(K0), LEN  :: L1=1
    REAL(K1) :: R(L1)=K1
  END TYPE 

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND :: K2=1
    INTEGER(K1), LEN  :: L2=1
    CHARACTER(L2) :: C(L2)=CHAR(K2)
    INTEGER(K2)   :: I(L2)=K2
    CLASS(DT2(K0,L0,K1,L0,K2,L2)), POINTER :: Ptr=>NULL()
  END TYPE 
  
  INTERFACE
    FUNCTION ExtFun(R,S,T, N)
    IMPORT
    IMPLICIT CLASS(DT0(1,:))(R)
    IMPLICIT CLASS(DT1(1,:,4,:))(S)
    IMPLICIT CLASS(DT2(1,:,4,:,8,:))(E,T)
    INTEGER     :: N
    ALLOCATABLE :: R(:)
    ALLOCATABLE :: S(:)
    ALLOCATABLE :: T(:)
    ALLOCATABLE :: ExtFun(:)
    type(dt2(1,:,4,:,8,:)) extFun
    END FUNCTION 
  END INTERFACE

  TYPE(DT0(1,3))         :: R(97)
  TYPE(DT1(1,3,4,5))     :: S(97)
  TYPE(DT2(1,3,4,5,8,7)) :: T(97)
  SAVE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT0(1,*)) :: Arg
  INTEGER ModFun
    ModFun = Arg%L0
  END FUNCTION

  END MODULE

  PROGRAM dtpImplicit9
  USE M

  IMPLICIT CLASS(DT0(1,:))(R)
  IMPLICIT CLASS(DT1(1,:,4,:))(S)
  IMPLICIT CLASS(DT2(1,:,4,:,8,:))(T, E)
  ALLOCATABLE :: RT(:)
  ALLOCATABLE :: ST(:)
  ALLOCATABLE :: TT(:), EF(:)
  INTEGER :: I
  type(dt2(1,:,4,:,8,:)) ef

  ALLOCATE(DT0(1,3) :: RT(97))
  ALLOCATE(DT1(1,3,4,5) :: ST(97))
  ALLOCATE(DT2(1,3,4,5,8,7) :: TT(97))

  EF = ExtFun(RT, ST, TT, 97)

  DO I=1, 97

    IF ( RT(I)%L0             .NE. R%L0         ) STOP 11

    IF ( SIZE( ST(I)%R )      .NE. S%L1         ) STOP 12
    IF ( ST(I)%R%KIND         .NE. S%K1         ) STOP 13
    IF ( ANY ( ST(I)%R        .NE. -S%K1      ) ) STOP 14

    IF ( SIZE( TT(I)%R )      .NE. T%L1         ) STOP 21
    IF ( TT(I)%R%KIND         .NE. T%K1         ) STOP 22
    IF ( ANY ( TT(I)%R        .NE. -T%K1      ) ) STOP 23
    IF ( SIZE( TT(I)%I )      .NE. T%L2         ) STOP 24
    IF ( TT(I)%I%KIND         .NE. T%K2         ) STOP 25
    IF ( ANY ( TT(I)%I        .NE. -T%K2      ) ) STOP 26
    IF ( SIZE( TT(I)%C )      .NE. T%L2         ) STOP 27
    IF ( TT(I)%C%LEN          .NE. T%L2         ) STOP 28
    IF ( ANY (TT(I)%C         .NE. CHAR(0)    ) ) STOP 29

    IF ( SIZE( EF(I)%R )      .NE. T%L1         ) STOP 51
    IF ( EF(I)%R%KIND         .NE. T%K1         ) STOP 52
    IF ( ANY ( EF(I)%R        .NE. -T%K1      ) ) STOP 53
    IF ( SIZE( EF(I)%I )      .NE. T%L2         ) STOP 54
    IF ( EF(I)%I%KIND         .NE. T%K2         ) STOP 55
    IF ( ANY ( EF(I)%I        .NE. -T%K2      ) ) STOP 56
    IF ( SIZE( EF(I)%C )      .NE. T%L2         ) STOP 57
    IF ( EF(I)%C%LEN          .NE. T%L2         ) STOP 58
    IF ( ANY (EF(I)%C         .NE. CHAR(0)    ) ) STOP 59

  END DO


  END

  FUNCTION ExtFun(RA,SA,TA, N)
  USE M, ONLY: DT0,DT1,DT2,R,S,T

  IMPLICIT CLASS(DT0(1,:))(R)
  IMPLICIT CLASS(DT1(1,:,4,:))(S)
  IMPLICIT CLASS(DT2(1,:,4,:,8,:))(E,T)
  INTEGER     :: N, I
  ALLOCATABLE :: RA(:)
  ALLOCATABLE :: SA(:)
  ALLOCATABLE :: TA(:)
  ALLOCATABLE :: ExtFun(:)

  type(dt2(1,:,4,:,8,:)) ExtFun

  IF ( .NOT. ALLOCATED(RA) ) STOP 91
  IF ( .NOT. ALLOCATED(SA) ) STOP 92
  IF ( .NOT. ALLOCATED(TA) ) STOP 93

  DO I=1, N

    IF ( RA(I)%ModFun()       .NE. R%L0         ) STOP 31

    IF ( SIZE( SA(I)%R )      .NE. S%L1         ) STOP 32
    IF ( SA(I)%R%KIND         .NE. S%K1         ) STOP 33
    IF ( ANY ( SA(I)%R        .NE. S%K1       ) ) STOP 34

    IF ( SIZE( TA(I)%R )      .NE. T%L1         ) STOP 41
    IF ( TA(I)%R%KIND         .NE. T%K1         ) STOP 42
    IF ( ANY ( TA(I)%R        .NE. T%K1       ) ) STOP 43
    IF ( SIZE( TA(I)%I )      .NE. T%L2         ) STOP 44
    IF ( TA(I)%I%KIND         .NE. T%K2         ) STOP 45
    IF ( ANY ( TA(I)%I        .NE. T%K2       ) ) STOP 46
    IF ( SIZE( TA(I)%C )      .NE. T%L2         ) STOP 47
    IF ( TA(I)%C%LEN          .NE. T%L2         ) STOP 48
    IF ( ANY (TA(I)%C         .NE. CHAR(T%K2) ) ) STOP 49

    SA(I)%R = -SA(I)%R
    TA(I)%R = -TA(I)%R
    TA(I)%I = -TA(I)%I
    TA(I)%C = CHAR(0)

  END DO
 
  ExtFun = TA

  END FUNCTION 

