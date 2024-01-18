!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 19, 2007
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
!*  TYPE IS ( derived-type-spec )
!*  for associate name
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE, PRIVATE :: DT0(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    PRIVATE
    INTEGER       :: I=K
  END TYPE

  TYPE, EXTENDS(DT0), PRIVATE :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=K
    INTEGER(2), LEN      :: LEN=K
    CHARACTER(LEN)       :: C=CHAR(KIND)
    REAL(K), PRIVATE     :: R=K
  END TYPE

  CLASS(DT0(8_8, L=:        )), POINTER       :: T3(:)
  CLASS(DT(8_8,  L=:,  LEN=:)), ALLOCATABLE   :: T4(:)

  CONTAINS

  SUBROUTINE Check()

  ALLOCATE( T3(10), SOURCE=DT(8, KIND=2_1, L=1_8)( C=CHAR(1)))

  SELECT TYPE ( T3 )
  TYPE IS (DT(8_8, KIND=2, L=*, LEN=*))
    IF ( T3%K               .NE.   8          ) ERROR STOP 11
    IF ( T3%L               .NE.   1          ) ERROR STOP 12
    IF ( T3%KIND            .NE.   2          ) ERROR STOP 13
    IF ( T3%LEN             .NE.   8          ) ERROR STOP 14
    IF ( ANY( T3%C          .NE.   CHAR(1)  ) ) ERROR STOP 17
  CLASS DEFAULT
    STOP 18
  END SELECT

  ALLOCATE( T4(10), SOURCE=DT(K=8, KIND=8, LEN=2)(C=CHAR(11)))

  SELECT TYPE ( T4 )
  TYPE IS (DT(8_8, KIND=8, L=*, LEN=*))
    IF ( T4%K               .NE.   8          ) ERROR STOP 21
    IF ( T4%L               .NE.   1          ) ERROR STOP 22
    IF ( T4%KIND            .NE.   8          ) ERROR STOP 23
    IF ( T4%LEN             .NE.   2          ) ERROR STOP 24
    IF ( ANY( T4%C          .NE.   CHAR(11) ) ) ERROR STOP 27
  CLASS DEFAULT
    STOP 28
  END SELECT

  END SUBROUTINE

  END MODULE

  MODULE M1

  TYPE, PRIVATE :: DT0(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
  END TYPE

  TYPE, EXTENDS(DT0) :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=K
    INTEGER(2), LEN      :: LEN=K
    INTEGER(KIND)        :: I=KIND
    CHARACTER(LEN)       :: C=CHAR(KIND)
    LOGICAL(K), PRIVATE  :: LL=.TRUE.
  END TYPE

  END MODULE

  PROGRAM dtParamTypeDecDT4
  USE M
  USE M1

  IMPLICIT NONE

  CLASS(DT(2_8,    L=:, LEN=1)),   POINTER       :: T7(:)
  CLASS(DT(2_8,    L=1, LEN=:)),   ALLOCATABLE   :: T8(:)

  CALL Check()

  ALLOCATE( T7(10), SOURCE=DT(2, LEN=1, KIND=2_1, L=1_8)( C=CHAR(1)))

  SELECT TYPE ( T7 )
  TYPE IS (DT(2, KIND=2, L=*, LEN=*))
    IF ( T7%K               .NE.   2          ) ERROR STOP 31
    IF ( T7%L               .NE.   1          ) ERROR STOP 32
    IF ( T7%KIND            .NE.   2          ) ERROR STOP 33
    IF ( T7%LEN             .NE.   1          ) ERROR STOP 34
    IF ( ANY( T7%C          .NE.   CHAR(1)  ) ) ERROR STOP 37
  CLASS DEFAULT
    STOP 38
  END SELECT

  ALLOCATE( T8(10), SOURCE=DT(K=2_8, KIND=2_1, LEN=2)(C=CHAR(11)))

  SELECT TYPE ( T8 )
  TYPE IS (DT(K=2, KIND=2, L=*, LEN=*))
    IF ( T8%K               .NE.   2          ) ERROR STOP 41
    IF ( T8%L               .NE.   1          ) ERROR STOP 42
    IF ( T8%KIND            .NE.   2          ) ERROR STOP 43
    IF ( T8%LEN             .NE.   2          ) ERROR STOP 44
    IF ( ANY( T8%C          .NE.   CHAR(11) ) ) ERROR STOP 47
  CLASS DEFAULT
    STOP 48
  END SELECT

  END

