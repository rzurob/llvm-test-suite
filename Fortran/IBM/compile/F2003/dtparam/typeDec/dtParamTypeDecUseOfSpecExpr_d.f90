!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecUseOfSpecExpr_d
!*
!*  DATE                       : May. 11, 2007
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
!*   A specification-expr in an array-spec, in a type-param-value in a declaration-type-spec corresponding to
!*   a length type parameter, or in a char-length in an entity-decl shall be an initialization expression unless
!*   it is in an interface body (12.3.2.1), the specification part of a subprogram, or the declaration-type-spec
!*   of a FUNCTION statement (12.5.2.1).
!*
!*  (336732)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K, L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    INTEGER :: I=K
  END TYPE
  INTEGER :: I=4

  END MODULE

  MODULE M1
  USE M

  TYPE(DT(K=1, L=I)) :: T1
  TYPE(DT(K=1, L=1)) :: T2(I)
  CHARACTER(I)       :: C1

  END MODULE

  PROGRAM dtParamTypeDecUseOfSpecExpr_d
  USE M

  TYPE(DT(K=1, L=I)) :: T3
  TYPE(DT(K=1, L=1)) :: T4(I)
  CHARACTER(I)       :: C2

  ! The following is ok

  INTERFACE

    SUBROUTINE S(J, T)
    IMPORT
      INTEGER :: J
      TYPE(DT(1,J)) :: T(J)
    END SUBROUTINE

    FUNCTION F(J)
    IMPORT
      INTEGER :: J
      TYPE(DT(1,J)) F
    END FUNCTION

  END INTERFACE

  END


