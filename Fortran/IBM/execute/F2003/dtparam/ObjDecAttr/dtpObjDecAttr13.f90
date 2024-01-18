!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr13
!*
!*  DATE                       : May. 28, 2007
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
!*  intent(out)
!*  -- the definition of components with the default initialization
!*
!*  (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER, KIND        :: K1=ABS(K0)
    INTEGER, LEN         :: L1=ABS(K0)
    CHARACTER(ABS(L1)+3) :: C1 = "DT1"
    CONTAINS
    PROCEDURE(ModFun), NOPASS, DEFERRED :: Proc
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND    :: K2=ABS(K1)
    INTEGER(K1), LEN     :: L2=ABS(K1)
    CHARACTER(ABS(L2))        :: C2=CHAR(ABS(K2))
    INTEGER(ABS(K2))          :: I=ABS(-K2)
    REAL   (ABS(K2))          :: R=ABS(-K2)
    LOGICAL(ABS(K2))          :: L=.TRUE._1
    COMPLEX(ABS(K2))          :: Z=(ABS(K1), ABS(K2))
    TYPE(DT0(ABS(K2), ABS(L2)))           :: T0
    TYPE(DT2(ABS(K2), ABS(L2),k2,k2,k2,k2)), POINTER  :: Ptr2
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun
  END TYPE

  CHARACTER(10) :: C

  CONTAINS

  FUNCTION ModFun(Arg)
  TYPE(DT2(2,*,2,*,8,*)), INTENT(OUT) :: Arg
  TYPE(DT2(2,2,2,2,8,8)) :: ModFun
     ModFun = DT2(2,2,2,2,8,8)(t0=dt0(8,8)(), ptr2=null())
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT2(2,*,2,*,8,*)), INTENT(OUT) :: Arg
  TYPE(DT2(2,2,2,2,8,8)) :: ExtFun

    IF ( Arg%K0          .NE.   2     ) STOP 11
    IF ( Arg%K1          .NE.   2     ) STOP 12
    IF ( Arg%K2          .NE.   8     ) STOP 13
    IF ( Arg%L0          .NE.   2     ) STOP 14
    IF ( Arg%L1          .NE.   2     ) STOP 15
    IF ( Arg%L2          .NE.   8     ) STOP 16

    IF ( Arg%C1          .NE.   "DT1"            ) STOP 10
    IF ( Arg%C2          .NE.   CHAR(Arg%K2)     ) STOP 17
    IF ( Arg%I           .NE.   Arg%K2           ) STOP 18
    IF ( Arg%R           .NE.   Arg%K2           ) STOP 19
    IF ( .NOT. Arg%L                             ) STOP 21
    IF ( Arg%Z           .NE.   (Arg%K1, Arg%K2) ) STOP 22
    IF ( Arg%T0%K0        .NE.   Arg%K2           ) STOP 23
    IF ( Arg%T0%L0        .NE.   Arg%L2           ) STOP 24
    IF ( ASSOCIATED( Arg%Ptr2 )                  ) STOP 25

    ExtFun%C2 = CHAR(Arg%K2+1)
    ExtFun%I = Arg%K2 + 1
    ExtFun%R = Arg%K2 + 1
    ExtFun%L = .true.
    ExtFun%Z =  (Arg%K1+1, Arg%K2+1)
    ExtFun%T0 = DT0(8, Arg%L2)()
    ExtFun%Ptr2 => NULL()

    C = "ExtFun"

  END FUNCTION

  PROGRAM dtpObjDecAttr13
  USE M

  PROCEDURE(ModFun) ExtFun
  !TYPE(DT2(-2,:,-2,:,-8,:)), POINTER :: Ptr
  TYPE(DT2(2,2,2,2,8,8)), TARGET :: Tar, arg

  Tar%C1 = ""
  Tar%I = 0
  Tar%R = 0.
  Tar%L = .FALSE.
  Tar%Z = (0.,0.)

  !Ptr => Tar

  arg = ExtFun(Tar)

  IF ( Arg%K0          .NE.   2     ) STOP 31
  IF ( Arg%K1          .NE.   2     ) STOP 32
  IF ( Arg%K2          .NE.   8     ) STOP 33
  IF ( Arg%L0          .NE.   2     ) STOP 34
  IF ( Arg%L1          .NE.   2     ) STOP 35
  IF ( Arg%L2          .NE.   8     ) STOP 36

  IF ( Arg%C1          .NE.   "DT1"                ) STOP 30
  IF ( Arg%C2          .NE.   CHAR(Arg%K2+1)       ) STOP 37
  IF ( Arg%I           .NE.   Arg%K2+1             ) STOP 38
  IF ( Arg%R           .NE.   Arg%K2+1             ) STOP 39
  IF ( .NOT. Arg%L                                 ) STOP 41
  IF ( Arg%Z           .NE.   (Arg%K1+1, Arg%K2+1) ) STOP 42
  IF ( Arg%T0%K0        .NE.   Arg%K2               ) STOP 43
  IF ( Arg%T0%L0        .NE.   Arg%L2               ) STOP 44
  IF ( ASSOCIATED( Arg%Ptr2 )                      ) STOP 45

  IF ( C  .NE.   "ExtFun"      ) STOP 46


  END



