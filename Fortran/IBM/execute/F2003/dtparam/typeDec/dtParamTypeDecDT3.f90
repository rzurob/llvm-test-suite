!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecDT3
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
!*  TYPE ( derived-type-spec ) 
!*  accessibility
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

  TYPE :: DT0(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    SEQUENCE
    INTEGER       :: I=K
  END TYPE

  TYPE :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=4
    INTEGER(2), LEN      :: LEN=1
    SEQUENCE
    INTEGER(KIND)        :: I=KIND
    CHARACTER(LEN)       :: C=CHAR(KIND)
    TYPE(DT0(KIND, LEN)) :: T=DT0(KIND, 1)()
  END TYPE

  TYPE(DT(KIND=2, LEN=1))   :: T1(1)  =  DT(KIND=2_1, LEN=1_8)() 
  TYPE(DT(8_8,    LEN=1))   :: T2(1)  =  DT(KIND=8_1, LEN=1)() 

  TYPE(DT(8_8, LEN=1)), POINTER       :: T3(:) 
  TYPE(DT(4_8, LEN=:)), ALLOCATABLE   :: T4(:) 

  END MODULE

  MODULE M1

  TYPE :: DT0(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    SEQUENCE
    INTEGER       :: I=K
  END TYPE

  TYPE :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=4
    INTEGER(2), LEN      :: LEN=1
    SEQUENCE
    INTEGER(KIND)        :: I=KIND
    CHARACTER(LEN)       :: C=CHAR(KIND)
    TYPE(DT0(KIND, LEN)) :: T=DT0(KIND, 1)()
  END TYPE

  END MODULE
 
  PROGRAM dtParamTypeDecDT3
  USE M, ONLY: T1,T2,T3,T4
  USE M1

  IMPLICIT NONE

  TYPE(DT(KIND=2, LEN=1))   :: T5(1)  =  DT(KIND=2_1, LEN=1_8)()
  TYPE(DT(2_8,    LEN=1))   :: T6(1)  =  DT(KIND=2_1, LEN=1)()

  TYPE(DT(2_8,    LEN=1)), POINTER       :: T7(:)
  TYPE(DT(2_8,    LEN=:)), ALLOCATABLE   :: T8(:)


  IF ( T1%KIND            .NE.   2          ) STOP 13
  IF ( T1%LEN             .NE.   1          ) STOP 14
  IF ( ANY( T1%C          .NE.   CHAR(2)  ) ) STOP 16

  IF ( T2%KIND            .NE.   8          ) STOP 23
  IF ( T2%LEN             .NE.   1          ) STOP 24
  IF ( ANY( T2%C          .NE.   CHAR(8)  ) ) STOP 26

  ALLOCATE( T3(10), SOURCE=DT(KIND=8_1, LEN=1_8)(I=-1, C=CHAR(1)))
  ALLOCATE( T4(10), SOURCE=DT(KIND=4_1, LEN=1)(I=11, C=CHAR(11)))

  IF ( T3%KIND            .NE.   8          ) STOP 33
  IF ( T3%LEN             .NE.   1          ) STOP 34
  IF ( ANY( T3%I          .NE.   -1       ) ) STOP 35
  IF ( ANY( T3%C          .NE.   CHAR(1)  ) ) STOP 36

  IF ( T4%KIND            .NE.   4          ) STOP 43
  IF ( T4%LEN             .NE.   1          ) STOP 44
  IF ( ANY( T4%I          .NE.   11       ) ) STOP 45
  IF ( ANY( T4%C          .NE.   CHAR(11) ) ) STOP 46


  IF ( T5%KIND            .NE.   2          ) STOP 13
  IF ( T5%LEN             .NE.   1          ) STOP 14
  IF ( ANY( T5%I          .NE.   2        ) ) STOP 15
  IF ( ANY( T5%C          .NE.   CHAR(2)  ) ) STOP 16

  IF ( T6%KIND            .NE.   2          ) STOP 23
  IF ( T6%LEN             .NE.   1          ) STOP 24
  IF ( ANY( T6%I          .NE.   2        ) ) STOP 25
  IF ( ANY( T6%C          .NE.   CHAR(2)  ) ) STOP 26

  ALLOCATE( T7(10), SOURCE=DT(KIND=2_1)(I=-1, C=CHAR(1)))
  ALLOCATE( T8(10), SOURCE=DT(KIND=2_1, LEN=1)(I=11, C=CHAR(11)))

  IF ( T7%KIND            .NE.   2          ) STOP 33
  IF ( T7%LEN             .NE.   1          ) STOP 34
  IF ( ANY( T7%I          .NE.   -1       ) ) STOP 35
  IF ( ANY( T7%C          .NE.   CHAR(1)  ) ) STOP 36

  IF ( T8%KIND            .NE.   2          ) STOP 43
  IF ( T8%LEN             .NE.   1          ) STOP 44
  IF ( ANY( T8%I          .NE.   11       ) ) STOP 45
  IF ( ANY( T8%C          .NE.   CHAR(11) ) ) STOP 46


  END

