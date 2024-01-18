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
!*  -- A specification inquiry
!*  -- a numeric inquiry function
!*     DIGITS/EPSILON/HUGE/MAXEXPONENT
!*     MINEXPONENT/PRECISION/RADIX/RANGE/TINY
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
    INTEGER(2), LEN      :: LEN=K
    CHARACTER(LEN)       :: C
    TYPE(DT0(KIND, LEN)) :: T!=DT0(KIND, LEN)()
  END TYPE

  TYPE(DT), SAVE :: T(2:3)

  CONTAINS
  SUBROUTINE ModSub()

  TYPE(DT(KIND=4,          LEN=DIGITS(1.0)))                :: T1(1)
  TYPE(DT(KIND=4,          L  =INT(EPSILON(1.0)*2**23) ))   :: T2(1)
  TYPE(DT(KIND=KIND(T%K),  LEN=HUGE(1_1)-126 ))             :: T3(1)
  TYPE(DT(KIND=4,          L  =MAXEXPONENT([2.0]) ) )       :: T4(1)


  IF ( T1%K               .NE.   4          ) STOP 11
  IF ( T1%L               .NE.   1          ) STOP 12
  IF ( T1%KIND            .NE.   4          ) STOP 13
  IF ( T1%LEN             .NE.   24         ) STOP 14
  IF ( ANY( T1%I          .NE.   4        ) ) STOP 15
  IF ( T1%T%K             .NE.   4          ) STOP 18
  IF ( T1%T%L             .NE.   24         ) STOP 19

  IF ( T2%K               .NE.   4          ) STOP 21
  IF ( T2%L               .NE.   1          ) STOP 22
  IF ( T2%KIND            .NE.   4          ) STOP 23
  IF ( T2%LEN             .NE.   4          ) STOP 24
  IF ( ANY( T2%I          .NE.   4        ) ) STOP 25
  IF ( T2%T%K             .NE.   4          ) STOP 28
  IF ( T2%T%L             .NE.   4          ) STOP 29

  IF ( T3%K               .NE.   4          ) STOP 31
  IF ( T3%L               .NE.   1          ) STOP 32
  IF ( T3%KIND            .NE.   4          ) STOP 33
  IF ( T3%LEN             .NE.   1          ) STOP 34
  IF ( ANY( T3%I          .NE.   4         )) STOP 35
  IF ( T3%T%K             .NE.   4          ) STOP 38
  IF ( T3%T%L             .NE.   1          ) STOP 39

  IF ( T4%K               .NE.   4          ) STOP 41
  IF ( T4%L               .NE.   128        ) STOP 42
  IF ( T4%KIND            .NE.   4          ) STOP 43
  IF ( T4%LEN             .NE.   4          ) STOP 44
  IF ( ANY( T4%I          .NE.   4         )) STOP 45
  IF ( T4%T%K             .NE.   4          ) STOP 48
  IF ( T4%T%L             .NE.   4          ) STOP 49

  END SUBROUTINE

  END MODULE

  PROGRAM dtParamTypeDecC501_62
  USE M

  CALL ModSub()
  CALL IntSub()

  CONTAINS

  SUBROUTINE IntSub()
  TYPE(DT(KIND=4,          LEN=MINEXPONENT([1.])+126))        :: T5(1)
  TYPE(DT(KIND=4,          L  =PRECISION([1.,2.]) ))          :: T6(1)
  TYPE(DT(KIND=KIND(T%K),  LEN=RADIX([1,2]) ))                :: T7(1)
  TYPE(DT(KIND=4,          L  = RANGE([1_2,2_2])))            :: T8(1)
  TYPE(DT(KIND=4,          LEN  = INT(TINY(1.)*2.**127)))     :: T9(1)

  IF ( T5%K               .NE.   4          ) STOP 51
  IF ( T5%L               .NE.   1          ) STOP 52
  IF ( T5%KIND            .NE.   4          ) STOP 53
  IF ( T5%LEN             .NE.   1          ) STOP 54
  IF ( ANY( T5%I          .NE.   4        ) ) STOP 55
  IF ( T5%T%K             .NE.   4          ) STOP 58
  IF ( T5%T%L             .NE.   1          ) STOP 59

  IF ( T6%K               .NE.   4          ) STOP 61
  IF ( T6%L               .NE.   6          ) STOP 62
  IF ( T6%KIND            .NE.   4          ) STOP 63
  IF ( T6%LEN             .NE.   4          ) STOP 64
  IF ( ANY( T6%I          .NE.   4        ) ) STOP 65
  IF ( T6%T%K             .NE.   4          ) STOP 68
  IF ( T6%T%L             .NE.   4          ) STOP 69

  IF ( T7%K               .NE.   4          ) STOP 71
  IF ( T7%L               .NE.   1          ) STOP 72
  IF ( T7%KIND            .NE.   4          ) STOP 73
  IF ( T7%LEN             .NE.   2          ) STOP 74
  IF ( ANY( T7%I          .NE.   4         )) STOP 75
  IF ( T7%T%K             .NE.   4          ) STOP 78
  IF ( T7%T%L             .NE.   2          ) STOP 79

  IF ( T8%K               .NE.   4          ) STOP 81
  IF ( T8%L               .NE.   4          ) STOP 82
  IF ( T8%KIND            .NE.   4          ) STOP 83
  IF ( T8%LEN             .NE.   4          ) STOP 84
  IF ( ANY( T8%I          .NE.   4         )) STOP 85
  IF ( T8%T%K             .NE.   4          ) STOP 88
  IF ( T8%T%L             .NE.   4          ) STOP 89

  IF ( T9%K               .NE.   4          ) STOP 91
  IF ( T9%L               .NE.   1          ) STOP 92
  IF ( T9%KIND            .NE.   4          ) STOP 93
  IF ( T9%LEN             .NE.   2          ) STOP 94
  IF ( ANY( T9%I          .NE.   4         )) STOP 95
  IF ( T9%T%K             .NE.   4          ) STOP 98
  IF ( T9%T%L             .NE.   2          ) STOP 99

  END SUBROUTINE

  END

