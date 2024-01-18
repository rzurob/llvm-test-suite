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
!*  -- A reference to a specification function where each argument is
!*     a restricted expression,
!*
!*  (336396)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    CHARACTER(L)  :: C
  END TYPE

  CONTAINS

  PURE FUNCTION ModFun(Arg)
  INTEGER :: ModFun
  INTEGER, INTENT(IN) :: Arg
    ModFun = Arg
  END FUNCTION

  SUBROUTINE ModSub(T1, L)
  INTEGER :: L
  TYPE(DT(K=4,          L=L)) :: T1(:)

  TYPE(DT(4,     L=ModFun(SIZE(T1))))                :: T2(SIZE(T1))
  TYPE(DT(4,     L=SIZE([ModFun(1), ModFun(2)])))    :: T3(SIZE(T1))

  IF ( T1%K               .NE.   4          )     STOP 11
  IF ( T1%L               .NE.   1          )     STOP 12
  IF ( ANY( ICHAR(T1%C)   .NE.   [(I,I=1,10)] ) ) STOP 13
  IF ( SIZE(T1)           .NE.   10         )     STOP 14

  IF ( T2%K               .NE.   4          )     STOP 21
  IF ( T2%L               .NE.   10         )     STOP 22
  IF ( LEN(T2%C)          .NE.   10         )     STOP 23
  IF ( SIZE(T2)           .NE.   10         )     STOP 24

  IF ( T3%K               .NE.   4          )     STOP 31
  IF ( T3%L               .NE.   2          )     STOP 32
  IF ( LEN(T3%C)          .NE.   2          )     STOP 33
  IF ( SIZE(T3)           .NE.   10         )     STOP 34

  END SUBROUTINE

  END MODULE


  PROGRAM dtParamTypeDecC501_8
  USE M

  CALL ModSub( [(DT(K=4_1, L=1)(C=CHAR(I)), I=1,10)], 1 )

  END

