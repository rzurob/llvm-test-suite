!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecDT2
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
!*  The basic syatax
!*  TYPE ( derived-type-spec )
!*  accessibility
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE, PRIVATE :: DT0(K,L)
    INTEGER, KIND :: K=1
    INTEGER, LEN  :: L=1
  END TYPE

  TYPE, EXTENDS(DT0) :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=K
    INTEGER(2), LEN      :: LEN=1
    INTEGER(KIND)        :: I=KIND
    CHARACTER(LEN)       :: C=CHAR(KIND)
    TYPE(DT0(KIND, LEN)) :: T =DT0(KIND, 1)()
  END TYPE

  TYPE(DT(KIND=2, LEN=1))   :: T1(1)  =  DT(KIND=2_1, LEN=1_8)()
  TYPE(DT(2_8,    LEN=1))   :: T2(1)  =  DT(K=2_8, KIND=2_1, LEN=1)()

  TYPE(DT(2_8,    L=:)),   POINTER       :: T3(:)
  TYPE(DT(2_8,    LEN=:)), ALLOCATABLE   :: T4(:)

  END MODULE

  PROGRAM dtParamTypeDecDT2
  USE M
  IMPLICIT NONE


  IF ( T1%K               .NE.   1          ) STOP 11
  IF ( T1%L               .NE.   1          ) STOP 12
  IF ( T1%KIND            .NE.   2          ) STOP 13
  IF ( T1%LEN             .NE.   1          ) STOP 14
  IF ( ANY( T1%I          .NE.   2        ) ) STOP 15
  IF ( ANY( T1%C          .NE.   CHAR(2)  ) ) STOP 16

  IF ( T2%K               .NE.   2          ) STOP 21
  IF ( T2%L               .NE.   1          ) STOP 22
  IF ( T2%KIND            .NE.   2          ) STOP 23
  IF ( T2%LEN             .NE.   1          ) STOP 24
  IF ( ANY( T2%I          .NE.   2        ) ) STOP 25
  IF ( ANY( T2%C          .NE.   CHAR(2)  ) ) STOP 26

  ALLOCATE( T3(10), SOURCE=DT(K=2,   KIND=2_1, L=1_8)(I=-1, C=CHAR(1)))
  ALLOCATE( T4(10), SOURCE=DT(K=2_8, KIND=2_1, LEN=1)(I=11, C=CHAR(11)))

  IF ( T3%K               .NE.   2          ) STOP 31
  IF ( T3%L               .NE.   1          ) STOP 32
  IF ( T3%KIND            .NE.   2          ) STOP 33
  IF ( T3%LEN             .NE.   1          ) STOP 34
  IF ( ANY( T3%I          .NE.   -1       ) ) STOP 35
  IF ( ANY( T3%C          .NE.   CHAR(1)  ) ) STOP 36

  IF ( T4%K               .NE.   2          ) STOP 41
  IF ( T4%L               .NE.   1          ) STOP 42
  IF ( T4%KIND            .NE.   2          ) STOP 43
  IF ( T4%LEN             .NE.   1          ) STOP 44
  IF ( ANY( T4%I          .NE.   11       ) ) STOP 45
  IF ( ANY( T4%C          .NE.   CHAR(11) ) ) STOP 46


  END

