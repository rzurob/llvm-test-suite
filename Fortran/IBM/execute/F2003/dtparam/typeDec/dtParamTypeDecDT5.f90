!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecDT5
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 19, 2007
!*  ORIGIN                     : Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : DERIVED TYPE PARAMETERS
!*
!*  SECONDARY FUNCTIONS TESTED : Data Object Declaration 
!*
!*  REFERENCE                  : Feature Number 289057
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  : -qfree=f90
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   
!*  The basic syatax  
!*  Allocation of a dummy argument with the length parameter being *
!*  
!*  (340741) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE, PRIVATE :: DT0(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=4
    PRIVATE
    INTEGER       :: I=K
  END TYPE

  TYPE, EXTENDS(DT0), PRIVATE :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=K
    INTEGER(2), LEN      :: LEN=K
    CHARACTER(LEN)       :: C=CHAR(KIND)
    TYPE(DT0(KIND, LEN)) :: T !=DT0(KIND, LEN)()
    REAL(K), PRIVATE     :: R=K
  END TYPE

  CLASS(DT(8_8, L=1)), POINTER       :: T3(:) 
  CLASS(DT(8_8, L=1)), ALLOCATABLE   :: T4(:) 

  CONTAINS

  SUBROUTINE Check0(T3, T4)
  CLASS(DT(8_8, L=*, LEN=*)),         POINTER      :: T3(:)
  CLASS(DT(8_8, KIND=8, L=*, LEN=*)), ALLOCATABLE  :: T4(:)
 
  ALLOCATE( DT(8_8, L=*, LEN=*) :: T3(10) )

  SELECT TYPE ( T3 )
  TYPE IS (DT(8_8, L=*, LEN=*))
    IF ( T3%K               .NE.   8          ) STOP 11
    IF ( T3%L               .NE.   1          ) STOP 12
    IF ( T3%KIND            .NE.   8          ) STOP 13
    IF ( T3%LEN             .NE.   8          ) STOP 14
    IF ( T3%T%K             .NE.   8          ) STOP 15
    IF ( T3%T%L             .NE.   8          ) STOP 16
    IF ( ANY( T3%C          .NE.   CHAR(8)  ) ) STOP 17
  CLASS DEFAULT
    STOP 18
  END SELECT

  !ALLOCATE( DT(8_8, KIND=8, L=*, LEN=*) :: T4(10) )
  ALLOCATE( T4(10), SOURCE=DT(8_8, L=1)(T=DT0(8,8)()) )

  SELECT TYPE ( T4 )
  TYPE IS (DT(8_8, KIND=8, L=*, LEN=*))
    IF ( T4%K               .NE.   8          ) STOP 21
    IF ( T4%L               .NE.   1          ) STOP 22
    IF ( T4%KIND            .NE.   8          ) STOP 23
    IF ( T4%LEN             .NE.   8          ) STOP 24
    IF ( T4%T%K             .NE.   8          ) STOP 25
    IF ( T4%T%L             .NE.   8          ) STOP 26
    IF ( ANY( T4%C          .NE.   CHAR(8) ) ) STOP 27
  CLASS DEFAULT
    STOP 28
  END SELECT

  END SUBROUTINE

  END MODULE

  MODULE M1

  TYPE :: DT0(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
  END TYPE

  TYPE, EXTENDS(DT0) :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=K
    INTEGER(2), LEN      :: LEN=K
    INTEGER(KIND)        :: I=KIND
    CHARACTER(LEN)       :: C=CHAR(KIND)
    TYPE(DT0(KIND, LEN)) :: T !=DT0(KIND, LEN)()
    LOGICAL(K), PRIVATE  :: LK=.TRUE.
  END TYPE

  END MODULE
 
  PROGRAM dtParamTypeDecDT5
  USE M
  USE M1

  IMPLICIT NONE

  CLASS(DT(2_8,    L=:, LEN=1)),   POINTER       :: T7(:)
  CLASS(DT(2_8,    L=1, LEN=:)),   ALLOCATABLE   :: T8(:)

  CALL Check0(T3, T4)

  ALLOCATE( T7(10), SOURCE=DT(K=2,   KIND=2_1, L=1_8, LEN=1)(C=CHAR(1),  T=DT0(2,1)()))
  ALLOCATE( T8(10), SOURCE=DT(K=2_8, KIND=2_1, LEN=2)(C=CHAR(11), T=DT0(2,2)()))
  CALL Check1(T7, T8)

  CONTAINS

  SUBROUTINE Check1(T7, T8)
  CLASS(DT(2_8,    L=*, LEN=*))   :: T7(10)
  CLASS(DT(2_8,    L=*, LEN=*))   :: T8(10)


  SELECT TYPE ( T7 )
  TYPE IS (DT(2, KIND=2, L=*, LEN=*))
    IF ( T7%K               .NE.   2          ) STOP 31
    IF ( T7%L               .NE.   1          ) STOP 32
    IF ( T7%KIND            .NE.   2          ) STOP 33
    IF ( T7%LEN             .NE.   1          ) STOP 34
    IF ( T7%T%K             .NE.   2          ) STOP 35
    IF ( T7%T%L             .NE.   1          ) STOP 36
    IF ( ANY( T7%C          .NE.   CHAR(1)  ) ) STOP 37
  CLASS DEFAULT
    STOP 38
  END SELECT


  SELECT TYPE ( T8 )
  TYPE IS (DT(K=2, KIND=2, L=*, LEN=*))
    IF ( T8%K               .NE.   2          ) STOP 41
    IF ( T8%L               .NE.   1          ) STOP 42
    IF ( T8%KIND            .NE.   2          ) STOP 43
    IF ( T8%LEN             .NE.   2          ) STOP 44
    IF ( T8%T%K             .NE.   2          ) STOP 45
    IF ( T8%T%L             .NE.   2          ) STOP 46
    IF ( ANY( T8%C          .NE.   CHAR(11) ) ) STOP 47
  CLASS DEFAULT
    STOP 48
  END SELECT

  END SUBROUTINE

  END

