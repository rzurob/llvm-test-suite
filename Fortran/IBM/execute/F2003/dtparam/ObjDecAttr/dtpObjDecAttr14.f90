!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtpObjDecAttr14
!*
!*  DATE                       : May. 30, 2007
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
!*  intent(inout)
!*
!*  (init expr issue)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER, KIND    :: K1=k0
    INTEGER, LEN     :: L1=k0
    CHARACTER(L1+3) :: C1 = "DT1"
    CONTAINS
    PROCEDURE(ModFun), NOPASS, DEFERRED :: Proc
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER, KIND    :: K2=k1
    INTEGER, LEN     :: L2=k1
    CHARACTER(L2)    :: C2=CHAR(K2)
    INTEGER(K2)      :: I=K2
    REAL   (K2)      :: R=K2
    LOGICAL(K2)      :: L=.TRUE._1
    COMPLEX(K2)      :: Z=(K1, K2)
    TYPE(DT0(K2, L2))           :: T0
    TYPE(DT2(K2, L2)), POINTER  :: Ptr2
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun
  END TYPE

  CHARACTER(10) :: C

  CONTAINS

  FUNCTION ModFun(Arg)
  TYPE(DT2(8,*,8,*,8,*)), INTENT(INOUT) :: Arg
  TYPE(DT2(8,Arg%L0,8,Arg%L1,8,Arg%L2)) :: ModFun
     ModFun = DT2(8,8,8,8,8,8)(t0=dt0(8,8)(),ptr2=null())
  END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  USE M
  TYPE(DT2(8,*,8,*,8,*)), INTENT(INOUT) :: Arg
  TYPE(DT2(8,Arg%L0,8,Arg%L1,8,Arg%L2)) :: ExtFun

    IF ( Arg%K0          .NE.   8     ) STOP 11
    IF ( Arg%K1          .NE.   8     ) STOP 12
    IF ( Arg%K2          .NE.   8     ) STOP 13
    IF ( Arg%L0          .NE.   8     ) STOP 14
    IF ( Arg%L1          .NE.   8     ) STOP 15
    IF ( Arg%L2          .NE.   8     ) STOP 16

    IF ( Arg%C1          .NE.   ''    ) STOP 17
    IF ( Arg%C2          .NE.   ''    ) STOP 17
    IF ( Arg%I           .NE.   0     ) STOP 18
    IF ( Arg%R           .NE.   0     ) STOP 19
    IF ( Arg%L                        ) STOP 21
    IF ( Arg%Z           .NE.   (2., 2.)         ) STOP 22
    IF ( Arg%T0%K0        .NE.   Arg%K2           ) STOP 23
    IF ( Arg%T0%L0        .NE.   Arg%L2           ) STOP 24
    IF ( .NOT. ASSOCIATED( Arg%Ptr2 )            ) STOP 25

    ExtFun%C1 = "12345678"
    ExtFun%C2 = "87654321"
    ExtFun%I =  1
    ExtFun%R =  1
    ExtFun%L = .TRUE.
    ExtFun%Z =  (1, 1)
    ExtFun%T0 = DT0(8, Arg%L2)()
    ExtFun%Ptr2 => NULL()

    C = "ExtFun"

  END FUNCTION

  PROGRAM dtpObjDecAttr14
  USE M

  PROCEDURE(ModFun) ExtFun
  TYPE(DT2(8,:,8,:,8,:)), POINTER :: Ptr
  TYPE(DT2(8,8,8,8,8,8)), TARGET :: Tar, arg

  Tar%C1 = ""
  Tar%C2 = ""
  Tar%I = 0
  Tar%R = 0.
  Tar%L = .FALSE.
  Tar%Z = (2.,2.)
  Tar%Ptr2 => Tar

  Ptr => Tar

  arg = ExtFun(Ptr)

  IF ( Arg%K0          .NE.   8     ) STOP 31
  IF ( Arg%K1          .NE.   8     ) STOP 32
  IF ( Arg%K2          .NE.   8     ) STOP 33
  IF ( Arg%L0          .NE.   8     ) STOP 34
  IF ( Arg%L1          .NE.   8     ) STOP 35
  IF ( Arg%L2          .NE.   8     ) STOP 36

  IF ( Arg%C1          .NE.   "12345678"    ) STOP 30
  IF ( Arg%C2          .NE.   "87654321"    ) STOP 37
  IF ( Arg%I           .NE.   1             ) STOP 38
  IF ( Arg%R           .NE.   1             ) STOP 39
  IF ( .not. Arg%L                          ) STOP 41
  IF ( Arg%Z           .NE.   (1, 1)        ) STOP 42
  IF ( Arg%T0%K0        .NE.   Arg%K2               ) STOP 43
  IF ( Arg%T0%L0        .NE.   Arg%L2               ) STOP 44
  IF ( ASSOCIATED( Arg%Ptr2 )                      ) STOP 45

  IF ( C  .NE.   "ExtFun"      ) STOP 46


  END



