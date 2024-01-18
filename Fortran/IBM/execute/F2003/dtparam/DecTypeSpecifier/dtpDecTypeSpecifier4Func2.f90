!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 17, 2007
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
!*  Type Specifier used in function
!*
!*  Type defined within function
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  FUNCTION ExtFun1(I,C) result (t)

  CHARACTER(*) C
  INTEGER  I

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    SEQUENCE
    INTEGER(K)    :: I=K
    CHARACTER(L)  :: C(L)
  END TYPE

  TYPE(DT(2,LEN(C))) t

  t = DT(2,LEN(C))(C=C, I=I)

  END FUNCTION

  PROGRAM dtpDecTypeSpecifier4Func2

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    SEQUENCE
    INTEGER(K)    :: I=K
    CHARACTER(L)  :: C(L)
  END TYPE

  INTERFACE

    FUNCTION ExtFun1(I,C)
      CHARACTER(*) C
      INTEGER  I

      TYPE :: DT(K,L)
        INTEGER, KIND :: K=4
        INTEGER, LEN  :: L=1
        SEQUENCE
        INTEGER(K)    :: I=K
        CHARACTER(L)  :: C(L)
      END TYPE

      TYPE(DT(2,LEN(C))) ExtFun1
    END FUNCTION

  END INTERFACE

  TYPE(DT(2,1)) :: B = DT(2,1)(C="Y", I=1)

  B = ExtFun1(C="X", I=-1)

  IF ( B%K               .NE.   2          ) ERROR STOP 11
  IF ( B%L               .NE.   1          ) ERROR STOP 12
  IF ( B%I%KIND          .NE.   2          ) ERROR STOP 13
  IF ( B%I               .NE.  -1          ) ERROR STOP 14
  IF ( B%C%LEN           .NE.   1          ) ERROR STOP 15
  IF ( SIZE(B%C)         .NE.   1          ) ERROR STOP 16
  IF ( ANY(B%C           .NE.   "X"      ) ) ERROR STOP 17

  END

