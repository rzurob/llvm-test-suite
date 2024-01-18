!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefAccess
!*
!*  DATE                       : Dec. 01, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Derived type definition
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
!*  The accessibility
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L

      INTEGER(K)   :: I
      CHARACTER(L) :: C

    CONTAINS
      PROCEDURE, PASS :: IntFun
    END TYPE

  CONTAINS

    FUNCTION IntFun(Arg)
    CLASS(DT(4,*)):: Arg
    TYPE(DT(4, 4)):: IntFun
      IntFun = Arg
    END FUNCTION

  END MODULE

  MODULE M1
  USE M, DT1 => DT, DT => DT
  PRIVATE DT1

  TYPE(DT1(4,4)) :: W=DT(4,4)(-2, "4321")

  END MODULE

  MODULE M2

    TYPE, PRIVATE :: DT2(K, L)
      INTEGER, KIND :: K
      INTEGER, LEN  :: L

      INTEGER(K)   :: I
      CHARACTER(L) :: C

    CONTAINS
      PROCEDURE, PASS :: IntFun
    END TYPE

    TYPE(DT2(4,4)) :: V=DT2(4,4)(-3, "4321"), Q2=DT2(4,4)(1, "")

  CONTAINS

    FUNCTION IntFun(Arg)
    CLASS(DT2(4,*)):: Arg
    TYPE(DT2(4, 4)):: IntFun
      IntFun = Arg
    END FUNCTION


  END MODULE

  PROGRAM dtParamTypeDefAccess
  USE M
  USE M1
  USE M2


  TYPE(DT(4,4))  :: T=DT(4,4)(-1, "1234"), Q=DT(4,4)(1, "")

  Q = T%IntFun()
  IF ( Q%I .NE. -1 )     STOP 11
  IF ( Q%C .NE. "1234" ) STOP 12

  Q = W%IntFun()
  IF ( W%I .NE. -2 )     STOP 21
  IF ( W%C .NE. "4321" ) STOP 22
  IF ( Q%I .NE. -2 )     STOP 23
  IF ( Q%C .NE. "4321" ) STOP 24

  Q2 = V%IntFun()
  IF ( V%I  .NE. -3 )     STOP 31
  IF ( V%C  .NE. "4321" ) STOP 32
  IF ( Q2%I .NE. -3 )     STOP 33
  IF ( Q2%C .NE. "4321" ) STOP 34

  END

