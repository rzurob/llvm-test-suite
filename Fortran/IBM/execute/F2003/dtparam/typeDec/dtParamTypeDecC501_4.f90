!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 30, 2007
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
!*  -- An array constructor where each element and each scalar-int-expr of
!*     each ac-implied-do control is a restricted expression
!*
!*  (340524)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE, PRIVATE :: DT0(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    INTEGER       :: I=K
  END TYPE

  TYPE, EXTENDS(DT0) :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=K
    INTEGER(2), LEN      :: LEN=K
    TYPE(DT0(KIND, LEN)) :: T!=DT0(KIND, LEN)()
  END TYPE

  TYPE(DT(KIND=4,    LEN=SIZE([1]))), SAVE :: T0(1)
  INTEGER :: I

  END MODULE

  PROGRAM dtParamTypeDecC501_4
  USE M
  TYPE(DT(KIND=4,       LEN=LBOUND(T0, 1))) :: T1(1)

  CALL IntSub(  )

  CONTAINS

  SUBROUTINE IntSub()

  TYPE(DT(4,     LEN=SIZE([(T0%L, I=1,100)])))  :: T2(1)
  TYPE(DT(4,       L=SIZE([(T1%I, I=1,100)])))  :: T3(1)


  IF ( T0%K               .NE.   4          ) STOP 41
  IF ( T0%L               .NE.   1          ) STOP 42
  IF ( T0%KIND            .NE.   4          ) STOP 43
  IF ( T0%LEN             .NE.   1          ) STOP 44
  IF ( ANY( T0%I          .NE.   4        ) ) STOP 45
  !IF ( T0%DT0%K           .NE.   4          ) STOP 48
  !IF ( T0%DT0%L           .NE.   1          ) STOP 49
  IF ( T0%K               .NE.   4          ) STOP 48
  IF ( T0%L               .NE.   1          ) STOP 49

  IF ( T1%K               .NE.   4          ) STOP 11
  IF ( T1%L               .NE.   1          ) STOP 12
  IF ( T1%KIND            .NE.   4          ) STOP 13
  IF ( T1%LEN             .NE.   1          ) STOP 14
  IF ( ANY( T1%I          .NE.   4        ) ) STOP 15
  IF ( T1%T%K             .NE.   4          ) STOP 18
  IF ( T1%T%L             .NE.   1          ) STOP 19

  IF ( T2%K               .NE.   4          ) STOP 21
  IF ( T2%L               .NE.   1          ) STOP 22
  IF ( T2%KIND            .NE.   4          ) STOP 23
  IF ( T2%LEN             .NE.   100        ) STOP 24
  IF ( ANY( T2%I          .NE.   4        ) ) STOP 25
  IF ( T2%T%K             .NE.   4          ) STOP 28
  IF ( T2%T%L             .NE.   100        ) STOP 29

  IF ( T3%K               .NE.   4          ) STOP 31
  IF ( T3%L               .NE.   100        ) STOP 32
  IF ( T3%KIND            .NE.   4          ) STOP 33
  IF ( T3%LEN             .NE.   4          ) STOP 34
  IF ( ANY( T3%I          .NE.   4        ) ) STOP 35
  IF ( T3%T%K             .NE.   4          ) STOP 38
  IF ( T3%T%L             .NE.   4          ) STOP 39

  END SUBROUTINE

  END

