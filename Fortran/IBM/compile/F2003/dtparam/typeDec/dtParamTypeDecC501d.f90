!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC501
!*
!*  DATE                       : Apr. 17, 2007
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
!*  C501 (R501) In a declaration-type-spec, every type-param-value that is
!*  not a colon or an asterisk shall be a specification-expr
!*  -- A constant or subobject of a constant
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC501

  TYPE :: DT0(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    INTEGER       :: I=K
  END TYPE

  TYPE, EXTENDS(DT0) :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=K
    INTEGER(2), LEN      :: LEN=K
    REAL(KIND)           :: R=KIND
    CHARACTER(LEN)       :: C=CHAR(KIND)
    TYPE(DT0(KIND, LEN)) :: T=DT0(KIND, 4)()
  END TYPE

  INTEGER   :: I
  REAL      :: R
  LOGICAL   :: L
  CHARACTER :: C

  TYPE(DT(KIND=I,    LEN=1)) :: T1
  TYPE(DT(KIND=4,    LEN=I)) :: T2

  TYPE(DT(K=4,       L=R))   :: T3
  TYPE(DT(K=R,       L=4))   :: T4

  TYPE(DT(K=4,       LEN=L)) :: T5
  TYPE(DT(K=L,       LEN=4)) :: T6

  TYPE(DT(KIND=4,    L=C))   :: T7
  TYPE(DT(KIND=C,    L=4))   :: T8


  END

