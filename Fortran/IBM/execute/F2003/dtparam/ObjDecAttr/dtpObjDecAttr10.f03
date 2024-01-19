!*********************************************************************
!*  ===================================================================
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
!*  The external attribute -- External procedures
!*
!*  (337336)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

  TYPE :: DT0(K0, L0)
    INTEGER, KIND :: K0=1
    INTEGER, LEN  :: L0=1
  END TYPE

  TYPE, ABSTRACT, EXTENDS(DT0)  :: DT1(K1, L1)
    INTEGER, KIND :: K1=K0
    INTEGER, LEN  :: L1=K0
    CHARACTER(L1+3) :: C1 = "DT1"
    CONTAINS
    PROCEDURE(ModFun), NOPASS, DEFERRED :: Proc
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND    :: K2=K1
    INTEGER(K1), LEN     :: L2=K1
    CHARACTER(L2)        :: C2=""
    INTEGER(MOD(K2, 9))  :: I
    REAL   (MOD(K2, 17)) :: R
    LOGICAL(MOD(K2, 9))  :: L
    COMPLEX(MOD(K2, 17)) :: Z
    TYPE(DT0(K2, L2))    :: T0
    TYPE(DT2(K2, L2)), POINTER  :: Ptr2
    CONTAINS
    PROCEDURE, NOPASS :: Proc => ModFun
  END TYPE

  CHARACTER(10) :: C

  CONTAINS

  FUNCTION ModFun(L, Arg)
  INTEGER :: L
  TYPE(DT0(2,L)) :: ModFun, Arg
    ModFun = Arg
    C = "ModFun"
  END FUNCTION

  END MODULE

  FUNCTION ExtFun()
  USE M
  !INTEGER :: L
  TYPE(DT0(2,2)) :: ExtFun
    ExtFun = DT0(2,2)()
    C = "ExtFun"
  END FUNCTION

  PROGRAM dtpObjDecAttr10
  USE M

  TYPE(DT0(2,2)) :: T, ExtFun

  EXTERNAL ExtFun

  INTEGER  :: L=2

  T = ExtFun()

  IF ( T%K0            .NE.   2          ) ERROR STOP 11
  IF ( T%L0            .NE.   2          ) ERROR STOP 12
  IF ( C               .NE.   "ExtFun"   ) ERROR STOP 13

  T = modFun(2,DT0(2,L)())
  IF ( C               .NE.   "ModFun"   ) ERROR STOP 20

  END



