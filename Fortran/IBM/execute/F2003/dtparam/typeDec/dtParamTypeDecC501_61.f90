!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC501_61
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 30, 2007
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
!*  C501 (R501) In a declaration-type-spec, every type-param-value that is 
!*  not a colon or an asterisk shall be a specification-expr
!*
!*  -- A specification inquiry 
!*  -- BIT_SIZE/LEN/KIND/NEW_LINE 
!*   
!*   
!*
!*  (336280) 
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
  INTEGER, PARAMETER :: One=1 

  END MODULE

  PROGRAM dtParamTypeDecC501_61
  CALL ExtSub()
  END 

  SUBROUTINE ExtSub()
  USE M

  TYPE(DT(KIND=4,          LEN=BIT_SIZE(One)))        :: T1(1)  
  TYPE(DT(KIND=4,          L  =LEN(T%C)+1 ))          :: T2(1)  
  TYPE(DT(KIND=KIND(T%K),  LEN=KIND(T%L)+2 ))         :: T3(1)  
  TYPE(DT(KIND=4,          L  =ICHAR(NEW_LINE("A")))) :: T4(1)  


  IF ( T1%K               .NE.   4          ) STOP 11
  IF ( T1%L               .NE.   1          ) STOP 12
  IF ( T1%KIND            .NE.   4          ) STOP 13
  IF ( T1%LEN             .NE.   32         ) STOP 14
  IF ( ANY( T1%I          .NE.   4        ) ) STOP 15
  IF ( T1%T%K             .NE.   4          ) STOP 18
  IF ( T1%T%L             .NE.   32         ) STOP 19

  IF ( T2%K               .NE.   4          ) STOP 21
  IF ( T2%L               .NE.   5          ) STOP 22
  IF ( T2%KIND            .NE.   4          ) STOP 23
  IF ( T2%LEN             .NE.   4          ) STOP 24
  IF ( ANY( T2%I          .NE.   4        ) ) STOP 25
  IF ( T2%T%K             .NE.   4          ) STOP 28
  IF ( T2%T%L             .NE.   4          ) STOP 29

  IF ( T3%K               .NE.   4          ) STOP 31
  IF ( T3%L               .NE.   1          ) STOP 32
  IF ( T3%KIND            .NE.   4          ) STOP 33
  IF ( T3%LEN             .NE.   6          ) STOP 34
  IF ( ANY( T3%I          .NE.   4         )) STOP 35
  IF ( T3%T%K             .NE.   4          ) STOP 38
  IF ( T3%T%L             .NE.   6          ) STOP 39

  IF ( T4%K               .NE.   4          ) STOP 41
  IF ( T4%L               .NE.   10         ) STOP 42
  IF ( T4%KIND            .NE.   4          ) STOP 43
  IF ( T4%LEN             .NE.   4          ) STOP 44
  IF ( ANY( T4%I          .NE.   4         )) STOP 45
  IF ( T4%T%K             .NE.   4          ) STOP 48
  IF ( T4%T%L             .NE.   4          ) STOP 49

  END SUBROUTINE

