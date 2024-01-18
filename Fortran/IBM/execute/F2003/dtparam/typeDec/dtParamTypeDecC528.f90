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
!*  C528 (R501) If the VALUE attribute is specified, the length type parameter values
!*  shall be omitted or specified by initialization expressions.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC528

  TYPE :: DT(K, L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    INTEGER(K)    :: I(L)=K
  END TYPE

  INTERFACE
    SUBROUTINE ExtSub(T1, T2)
      TYPE :: DT(K, L)
        INTEGER, KIND :: K=4
        INTEGER, LEN  :: L=4
        SEQUENCE
        INTEGER(K)    :: I(L)=K
      END TYPE
      TYPE(DT(K=1)), VALUE  :: T1
      TYPE(DT(2, L=3)), VALUE  :: T2
    END SUBROUTINE
  END INTERFACE

  CALL ExtSub(DT(K=1)(I=1), DT(K=2, L=3)(I=3))

  END

  SUBROUTINE ExtSub(T1, T2)

  TYPE :: DT(K, L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    SEQUENCE
    INTEGER(K)    :: I(L)=K
  END TYPE


  TYPE(DT(K=1)), VALUE  :: T1
  TYPE(DT(2, L=3)), VALUE  :: T2

  IF ( T1%L       .NE. 4  ) STOP 11
  IF ( ANY( T1%I  .NE. 1 )) STOP 12
  IF ( T1%I%KIND  .NE. 1  ) STOP 13
  IF ( SIZE(T1%I) .NE. 4  ) STOP 14

  IF ( T2%L       .NE. 3  ) STOP 21
  IF ( ANY( T2%I  .NE. 3 )) STOP 22
  IF ( T2%I%KIND  .NE. 2  ) STOP 23
  IF ( SIZE(T2%I) .NE. 3  ) STOP 24

  END SUBROUTINE



