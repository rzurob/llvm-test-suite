!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jun. 27, 2007
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED CLASS PARAMETERS
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
!*  -- The implicit statement
!*   on IO statements
!*
!*  (ICE)
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
    REAL(K1) :: R(L1)=[1,2,3,4,5]
  END TYPE

  TYPE, EXTENDS(DT1) :: DT2(K2,L2)
    INTEGER(K1), KIND :: K2=1
    INTEGER(K1), LEN  :: L2=1
    CHARACTER(L2) :: C(L2)=[(CHAR(I),I=1,7)]
    INTEGER(K2)   :: I(L2)=[(-I,I=1,7)]
  END TYPE

  TYPE(DT0(1,3))         :: R(97)
  TYPE(DT1(1,3,4,5))     :: S(97)
  TYPE(DT2(1,3,4,5,8,7)) :: T(97)
  SAVE

  CONTAINS

  FUNCTION ModFun(Arg)
  CLASS(DT0(1,*)) :: Arg
  INTEGER ModFun
    ModFun = -Arg%L0
  END FUNCTION

  END MODULE

  PROGRAM dtpImplicit13
  USE M

  IMPLICIT TYPE(DT0(1,3))(R)
  IMPLICIT TYPE(DT1(1,3,4,5))(S)
  IMPLICIT TYPE(DT2(1,3,4,5,8,7))(T)


  PRINT*, R(1)%ModFun()
  PRINT*, S(1)
  PRINT*, T(1)


  END

