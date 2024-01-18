!*********************************************************************
!*  ===================================================================
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
!*  If a type parameter in a declaration-type-spec or in a char-length in an entity-decl is defined by an
!*  expression that is not an initialization expression, the type parameter value is established on entry to
!*  the procedure and is not affected by any redefinition or undefinition of the variables in
!*  the specification expression during execution of the procedure.
!*
!*  ()
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

  PROGRAM dtParamTypeDecUseOfSpecExpr1
  USE M
  IMPLICIT NONE

  TYPE (DT(2,1000)) T(1000)

  CALL S(100, [(DT(1,100)(I), I=1, 100)])

  T = F(1000, [(DT(2,1000)(I), I=1, 1000)])

  IF (T%K        .NE. 2   )                 STOP 31
  IF (T%L        .NE. 1000 )                STOP 32
  IF (SIZE(T)    .NE. 1000 )                STOP 33
  IF (ANY (T%I   .NE. [(I,I=1000, 1,-1)] )) STOP 34

  CONTAINS

  SUBROUTINE S(J, T)
  USE M
  INTEGER :: J
  TYPE(DT(1,J)) :: T(J)

  J = 0

  IF (T%K        .NE. 1   )              STOP 11
  IF (T%L        .NE. 100 )              STOP 12
  IF (SIZE(T)    .NE. 100 )              STOP 13
  IF (ANY (T%I   .NE. [(I,I=1, 100 )] )) STOP 14

  END SUBROUTINE

  FUNCTION F(J, T)
  !FUNCTION F(J, T)
  USE M
  INTEGER ::K, J
  TYPE(DT(2,J)) :: T(J)
  TYPE(DT(2,J)) :: F
  DIMENSION  :: F(J)

  K = J
  J = -1000

  IF (T%K        .NE. 2   )              STOP 21
  IF (T%L        .NE. 1000 )             STOP 22
  IF (SIZE(T)    .NE. 1000 )             STOP 23
  IF (ANY (T%I   .NE. [(I,I=1, 1000)] )) STOP 24

  F = T(K:1:-1)

  END FUNCTION

  END

