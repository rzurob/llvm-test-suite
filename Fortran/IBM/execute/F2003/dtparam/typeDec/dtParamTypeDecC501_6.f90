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
!*  -- A specification inquiry -- an array inquiry function
!*  -- LBOUND/SHAPE/SIZE/UBOUND
!*
!*  ()
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
    INTEGER(2), LEN      :: LEN=1
    TYPE(DT0(KIND, LEN)) :: T!=DT0(KIND, 1)()
  END TYPE

  TYPE(DT), SAVE :: T(2:3)
  INTEGER :: I
  type(dt0(4,1)), save :: t01

  END MODULE

  PROGRAM dtParamTypeDecC501_6
  USE M

  CALL IntSub()

  CONTAINS
  SUBROUTINE IntSub()

  TYPE(DT(KIND=4,    LEN=SIZE([DT(4, 1)(-T(2)%I, t01)])))     :: T1(1)
  TYPE(DT(KIND=4,    L  =LBOUND(T,1) ))   :: T2(1)
  TYPE(DT(KIND=4,    LEN=UBOUND(T,1) ))   :: T3(1)
  TYPE(DT(KIND=4,    L  =SIZE(SHAPE(T)))) :: T4(1)


  IF ( T1%K               .NE.   4          ) ERROR STOP 11
  IF ( T1%L               .NE.   1          ) ERROR STOP 12
  IF ( T1%KIND            .NE.   4          ) ERROR STOP 13
  IF ( T1%LEN             .NE.   1          ) ERROR STOP 14
  IF ( ANY( T1%I          .NE.   4       ) ) ERROR STOP 15
  IF ( T1%T%K             .NE.   4          ) ERROR STOP 18
  IF ( T1%T%L             .NE.   1          ) ERROR STOP 19

  IF ( T2%K               .NE.   4          ) ERROR STOP 21
  IF ( T2%L               .NE.   2          ) ERROR STOP 22
  IF ( T2%KIND            .NE.   4          ) ERROR STOP 23
  IF ( T2%LEN             .NE.   1          ) ERROR STOP 24
  IF ( ANY( T2%I          .NE.   4        ) ) ERROR STOP 25
  IF ( T2%T%K             .NE.   4          ) ERROR STOP 28
  IF ( T2%T%L             .NE.   1          ) ERROR STOP 29

  IF ( T3%K               .NE.   4          ) ERROR STOP 31
  IF ( T3%L               .NE.   1          ) ERROR STOP 32
  IF ( T3%KIND            .NE.   4          ) ERROR STOP 33
  IF ( T3%LEN             .NE.   3          ) ERROR STOP 34
  IF ( ANY( T3%I          .NE.   4         )) ERROR STOP 35
  IF ( T3%T%K             .NE.   4          ) ERROR STOP 38
  IF ( T3%T%L             .NE.   3          ) ERROR STOP 39

  IF ( T4%K               .NE.   4          ) ERROR STOP 41
  IF ( T4%L               .NE.   1          ) ERROR STOP 42
  IF ( T4%KIND            .NE.   4          ) ERROR STOP 43
  IF ( T4%LEN             .NE.   1          ) ERROR STOP 44
  IF ( ANY( T4%I          .NE.   4         )) ERROR STOP 45
  IF ( T4%T%K             .NE.   4          ) ERROR STOP 48
  IF ( T4%T%L             .NE.   1          ) ERROR STOP 49

  END SUBROUTINE
  END

