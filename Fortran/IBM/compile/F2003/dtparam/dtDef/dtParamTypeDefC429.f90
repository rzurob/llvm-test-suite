!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefC429
!*
!*  DATE                       : Nov. 30, 2005
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
!*  C429 (R429) If EXTENDS appears, SEQUENCE shall not appear.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE, ABSTRACT :: DT(K0)
      INTEGER, KIND :: K0=0
    END TYPE

    TYPE, EXTENDS(DT), PRIVATE :: DT1(K, L)
      INTEGER, KIND :: K=2
      INTEGER, LEN  :: L=1
      PRIVATE
      SEQUENCE
      INTEGER(K) :: I
    END TYPE

    TYPE :: DT2(K0)
      INTEGER, KIND :: K0
      SEQUENCE
    END TYPE

    TYPE, EXTENDS(DT), PRIVATE :: DT3(K, L)
      INTEGER, KIND :: K=2
      INTEGER, LEN  :: L=1
      TYPE(DT2(K+L)):: Comp
      INTEGER(K) :: I
    END TYPE

    TYPE, PUBLIC, EXTENDS(DT2) :: DT4(K, L)
      INTEGER, KIND :: K=2
      INTEGER, LEN  :: L=1
    END TYPE

    TYPE, BIND(C) :: DT5
    END TYPE

    TYPE, EXTENDS(DT5) :: DT6(K, L)
      INTEGER, KIND :: K=2
      INTEGER, LEN  :: L=1
    END TYPE

  END MODULE

  PROGRAM dtParamTypeDefC429

  END

