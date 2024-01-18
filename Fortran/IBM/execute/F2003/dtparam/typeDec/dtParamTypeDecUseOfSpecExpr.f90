!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecUseOfSpecExpr
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : May. 11, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!*  
!*   A specification-expr in an array-spec, in a type-param-value in a declaration-type-spec corresponding to
!*   a length type parameter, or in a char-length in an entity-decl shall be an initialization expression unless 
!*   it is in an interface body (12.3.2.1), the specification part of a subprogram, or the declaration-type-spec 
!*   of a FUNCTION statement (12.5.2.1). 
!* 
!*
!*     
!*
!*  (340828) 
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

  PROGRAM dtParamTypeDecUseOfSpecExpr
  USE M

  INTERFACE 

    SUBROUTINE S(J, T)
    IMPORT 
      INTEGER :: J
      TYPE(DT(1,J)) :: T(J) 
    END SUBROUTINE

    FUNCTION F(J,T)
    IMPORT
      INTEGER :: J
      TYPE(DT(2,J)) :: T(J), F
      DIMENSION  :: F(j)
      !TYPE(DT(2,J)) F(J)
    END FUNCTION

  END INTERFACE

  TYPE (DT(2,1000)) T(1000)

  CALL S(100, [(DT(1,100)(I), I=1, 100)])

  T = F(1000, [(DT(2,1000)(I), I=1, 1000)])

  IF (T%K        .NE. 2   )                 STOP 31
  IF (T%L        .NE. 1000 )                STOP 32
  IF (SIZE(T)    .NE. 1000 )                STOP 33
  IF (ANY (T%I   .NE. [(I,I=1000, 1,-1)] )) STOP 34

  END

  SUBROUTINE S(J, T)
  USE M
  INTEGER :: J
  TYPE(DT(1,J)) :: T(J) 

  IF (T%K        .NE. 1   )              STOP 11
  IF (T%L        .NE. 100 )              STOP 12
  IF (SIZE(T)    .NE. 100 )              STOP 13
  IF (ANY (T%I   .NE. [(I,I=1, 100 )] )) STOP 14
  
  END SUBROUTINE

  FUNCTION F(J, T)
  !FUNCTION F(J, T)
  USE M 
  INTEGER :: J
  TYPE(DT(2,J)) :: T(J) , F
  !TYPE(DT(2,J)) :: F(J) 
  DIMENSION  :: F(J)

  IF (T%K        .NE. 2   )              STOP 21
  IF (T%L        .NE. 1000 )             STOP 22
  IF (SIZE(T)    .NE. 1000 )             STOP 23
  IF (ANY (T%I   .NE. [(I,I=1, 1000)] )) STOP 24
  
  F = T(J:1:-1)

  END FUNCTION

