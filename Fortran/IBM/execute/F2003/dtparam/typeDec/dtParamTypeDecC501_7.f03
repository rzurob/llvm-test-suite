!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May. 02, 2007
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
!*
!*  -- A reference to any other standard intrinsic function where each argument
!*     is a restricted expression
!*    MAXVAL/MINVAL
!*
!*  (340649)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    CHARACTER(L)  :: C
  END TYPE

  CONTAINS

  SUBROUTINE ModSub(T1)
  TYPE(DT(K=4,          L=*)) :: T1(:)

  TYPE(DT(4,     L=MAXVAL(ICHAR(T1%C),   1)))  :: T2(SIZE(T1))
  TYPE(DT(4,     L=MINVAL([len(T1%C),99], 1)))  :: T3(SIZE(T1))

  IF ( T1%K               .NE.   4          )     ERROR STOP 11
  IF ( T1%L               .NE.   1          )     ERROR STOP 12
  IF ( ANY( ICHAR(T1%C)   .NE.   [(I,I=1,10)] ) ) ERROR STOP 13
  IF ( SIZE(T1)           .NE.   10         )     ERROR STOP 14

  IF ( T2%K               .NE.   4          )     ERROR STOP 21
  IF ( T2%L               .NE.   10         )     ERROR STOP 22
  IF ( LEN(T2%C)          .NE.   10         )     ERROR STOP 23
  IF ( SIZE(T2)           .NE.   10         )     ERROR STOP 24

  IF ( T3%K               .NE.   4          )     ERROR STOP 31
  IF ( T3%L               .NE.   1          )     ERROR STOP 32
  IF ( LEN(T3%C)          .NE.   1          )     ERROR STOP 33
  IF ( SIZE(T3)           .NE.   10         )     ERROR STOP 34

  END SUBROUTINE
  END MODULE


  PROGRAM dtParamTypeDecC501_7
  USE M

  CALL ModSub( [(DT(K=4_1, L=1)(C=CHAR(I)), I=1,10)] )

  END
