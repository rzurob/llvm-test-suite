!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr18
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
!*  PARAMETER
!*
!*  (ICE-337445)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
    CONTAINS
    PROCEDURE, PASS(Obj) :: Proc0 => CheckDT0
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
    PROCEDURE, PASS(Obj) :: Proc2 => CheckDT2
  END TYPE

  CONTAINS

  SUBROUTINE CheckDT0(Obj,Arg)
  CLASS(DT0(8,*)) :: Obj
  TYPE (DT0(8,*)) :: Arg
    IF ( Arg%K0   .NE.   Obj%K0     ) STOP 11
    IF ( Arg%L0   .NE.   Obj%L0     ) STOP 12
  END SUBROUTINE

  SUBROUTINE CheckDT2(Obj,Arg)
  CLASS(DT2(8,*,8,*,8,*)) :: Obj
  TYPE (DT2(8,*,8,*,8,*)) :: Arg

    IF ( Arg%K0   .NE.   Obj%K0     ) STOP 41
    IF ( Arg%L0   .NE.   Obj%L0     ) STOP 42
    IF ( Arg%K1   .NE.   Obj%K1     ) STOP 43
    IF ( Arg%L1   .NE.   Obj%L1     ) STOP 44
    IF ( Arg%K2   .NE.   Obj%K2     ) STOP 45
    IF ( Arg%L2   .NE.   Obj%L2     ) STOP 46

    IF ( Arg%C1   .NE.   Obj%C1     ) STOP 47
    IF ( Arg%C2   .NE.   Obj%C2     ) STOP 48
    IF ( Arg%I    .NE.   Obj%I      ) STOP 49
    IF ( Arg%R    .NE.   Obj%R      ) STOP 50
    IF ( Arg%L    .nEQV.  Obj%L      ) STOP 51
    IF ( Arg%Z    .NE.   Obj%Z      ) STOP 52
    IF ( Arg%T0%K0.NE.   Obj%T0%K0  ) STOP 53
    IF ( Arg%T0%L0.NE.   Obj%T0%L0  ) STOP 54

    IF ( ASSOCIATED( Arg%Ptr2) .nEQV. ASSOCIATED( Obj%Ptr2)  ) STOP 55
    IF ( Arg%Ptr2%K2           .NE.   Obj%Ptr2%K2           ) STOP 56
    IF ( Arg%Ptr2%L2           .NE.   Obj%Ptr2%L2           ) STOP 37

  END SUBROUTINE

  END MODULE

  PROGRAM dtpObjDecAttr18
  USE M

  TYPE(DT2(8,3,8,3,8,3)), TARGET :: Tar

  TYPE(DT0(8,2)),  PARAMETER :: T1=DT0(8,2)()
  TYPE(DT0(8,2)),  PARAMETER :: T2=T1
  TYPE(DT0(8,2)),  PARAMETER :: T3=T1

  TYPE(DT2(8,3,8,3,8,3)), PARAMETER ::             &
    T4=DT2(8,3,8,3,8,3)        (                   &
                                  C1 = "XYZ",      &
                                  C2 = "ZYX",      &
                                   I = 1234,       &
                                   R = 4321.,      &
                                   L = .TRUE.,     &
                                   Z = (1.,-1.),   &
                                   Ptr2 = null(),     &
                                   T0=DT0(8,3)()   )
  TYPE(DT2(8,3,8,3,8,3)), PARAMETER :: T5=T4, T6=T5

  CALL T1%proc0(T1)
  CALL T1%proc0(T2)
  CALL T2%proc0(T3)

  CALL T4%proc2(T4)
  CALL T4%proc2(T5)
  CALL T5%proc2(T6)

  END



