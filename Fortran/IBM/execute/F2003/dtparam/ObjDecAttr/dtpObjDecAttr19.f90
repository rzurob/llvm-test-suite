!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr19
!*
!*  DATE                       : May. 31, 2007
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
!*  PARAMETER -- array
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    CONTAINS
    PROCEDURE, PASS(Obj) ::  CheckDT0
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER(K0), KIND    :: K1=K0
    INTEGER(K0), LEN     :: L1=K0
    CHARACTER(L1+3) :: C1 = "DT1"
    !CONTAINS
    !PROCEDURE(CHECKDT1), NOPASS, DEFERRED :: Proc
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
    TYPE(DT2(K2, L2)), POINTER  :: Ptr2
    CONTAINS
    PROCEDURE, PASS(Obj) ::  CheckDT2
  END TYPE

  INTEGER, PARAMETER :: N=67
  INTEGER :: I


  TYPE(DT0(1,2)),  PARAMETER :: T1(N)=[(DT0(1,2)(), I=1,N)]

  TYPE(DT2(8,3,8,3,8,3)), PARAMETER ::             &
    T4(N)=[(DT2(8,3,8,3,8,3)   (                   &
                                  C1 = "XYZ",      &
                                  C2 = "ZYX",      &
                                   I = 1234,       &
                                   R = 4321.,      &
                                   L = .TRUE.,     &
                                   Z = (1.,-1.),   &
                                   Ptr2 = NULL(),  &
                                   T0=DT0(8,3)() ) , I=1,N)]

  CONTAINS

  ELEMENTAL FUNCTION CheckDT0(Obj,Arg)
  CLASS(DT0(1,*)), INTENT(IN) :: Obj
  TYPE (DT0(1,*)), INTENT(IN) :: Arg
  LOGICAL                     :: CheckDT0

  CheckDT0 = .false.
    IF ( Arg%K0   .NE.   Obj%K0     ) CheckDT0 = .TRUE. !STOP 11
    IF ( Arg%L0   .NE.   Obj%L0     ) CheckDT0 = .TRUE. !STOP 12

  END FUNCTION

  ELEMENTAL FUNCTION CheckDT2(Obj,Arg)
  CLASS(DT2(8,*,8,*,8,*)), INTENT(IN) :: Obj
  TYPE (DT2(8,*,8,*,8,*)), INTENT(IN) :: Arg
  LOGICAL                             :: CheckDT2

    CheckDT2 = .false.

    IF ( Arg%K0   .NE.   Obj%K0     ) CheckDT2 = .TRUE.  ! STOP 41
    IF ( Arg%L0   .NE.   Obj%L0     ) CheckDT2 = .TRUE.  ! STOP 42
    IF ( Arg%K1   .NE.   Obj%K1     ) CheckDT2 = .TRUE.  ! STOP 43
    IF ( Arg%L1   .NE.   Obj%L1     ) CheckDT2 = .TRUE.  ! STOP 44
    IF ( Arg%K2   .NE.   Obj%K2     ) CheckDT2 = .TRUE.  ! STOP 45
    IF ( Arg%L2   .NE.   Obj%L2     ) CheckDT2 = .TRUE.  ! STOP 46

    IF ( Arg%C1   .NE.   Obj%C1     ) CheckDT2 = .TRUE.  ! STOP 47
    IF ( Arg%C2   .NE.   Obj%C2     ) CheckDT2 = .TRUE.  ! STOP 48
    IF ( Arg%I    .NE.   Obj%I      ) CheckDT2 = .TRUE.  ! STOP 49
    IF ( Arg%R    .NE.   Obj%R      ) CheckDT2 = .TRUE.  ! STOP 50
    IF ( Arg%L    .nEQV.  Obj%L      ) CheckDT2 = .TRUE.  ! STOP 51
    IF ( Arg%Z    .NE.   Obj%Z      ) CheckDT2 = .TRUE.  ! STOP 52
    IF ( Arg%T0%K0.NE.   Obj%T0%K0  ) CheckDT2 = .TRUE.  ! STOP 53
    IF ( Arg%T0%L0.NE.   Obj%T0%L0  ) CheckDT2 = .TRUE.  ! STOP 54

    IF ( ASSOCIATED( Arg%Ptr2) .nEQV. ASSOCIATED( Obj%Ptr2)     ) CheckDT2 = .TRUE.  ! STOP 55
    IF ( Arg%Ptr2%K2           .NE.   Obj%Ptr2%K2           ) CheckDT2 = .TRUE.  ! STOP 56
    IF ( Arg%Ptr2%L2           .NE.   Obj%Ptr2%L2           ) CheckDT2 = .TRUE.  ! STOP 37

  END FUNCTION

  END MODULE

  PROGRAM dtpObjDecAttr19
  USE M

  TYPE(DT0(1,2)),  PARAMETER :: T2(N)=[T1]
  TYPE(DT0(1,2)),  PARAMETER :: T3(N)=T1

  TYPE(DT2(8,3,8,3,8,3)), PARAMETER :: T5(N)=T4, T6(N)=[T5]

  IF (ANY( T1%CheckDT0(T1) ) ) STOP 11
  IF (ANY( T1%CheckDT0(T2) ) ) STOP 12
  IF (ANY( T2%CheckDT0(T3) ) ) STOP 13

  IF (ANY( T4%CheckDT2(T4) ) ) STOP 14
  IF (ANY( T4%CheckDT2(T5) ) ) STOP 15
  IF (ANY( T5%CheckDT2(T6) ) ) STOP 16

  END


