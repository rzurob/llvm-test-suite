!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDefSyntax4
!*
!*  DATE                       : Nov. 28, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Syntax
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
!*  the order of type param def stmt in derived type def
!*
!*  (Passing?)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT1(K)
    SEQUENCE
    INTEGER, KIND :: K
  END TYPE

  TYPE :: DT2(K)
    PRIVATE
    INTEGER, KIND :: K
  END TYPE

  TYPE :: DT3(L)
    SEQUENCE
    INTEGER, LEN :: L
  END TYPE

  TYPE :: DT4(L)
    PRIVATE
    INTEGER, LEN :: L
  END TYPE

  TYPE :: DT5(K)
    INTEGER   :: I
    INTEGER, KIND :: K
  END TYPE

  TYPE :: DT6(K)
    CONTAINS
    PROCEDURE, NOPASS :: P => S
    INTEGER, KIND :: K
  END TYPE

  INTERFACE
    SUBROUTINE S()
    END SUBROUTINE
  END INTERFACE

  END MODULE

  PROGRAM dtParamTypeDefSyntax4
  END

