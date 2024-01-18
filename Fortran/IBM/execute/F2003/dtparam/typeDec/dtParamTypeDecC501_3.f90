!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC501_3
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Apr. 24, 2007
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
!*  -- An object designator with a base object that is made accessible 
!*     by use association or host association
!*   
!*
!*  () 
!   JX:2008-06-11: try to fix it. Only can guess what's the original test
!   intention.
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
    REAL(KIND)           :: R=KIND
    CHARACTER(LEN)       :: C=CHAR(KIND)
    TYPE(DT0(KIND, LEN)) :: T!=DT0(KIND, KIND-3)()
  END TYPE

  TYPE(DT(KIND=4,          LEN=1)), SAVE :: T0(1)  
  INTEGER :: I
  COMMON /A/I

  type(dt0(4,1)), save :: t01

  END MODULE

  PROGRAM dtParamTypeDecC501_3
  USE M
  TYPE(DT(KIND=4,          LEN=1)) :: T1(1)  

  i = 1
  CALL IntSub( [DT(KIND=4_1, LEN=1_8)(t=t01)] )

  CONTAINS

  SUBROUTINE IntSub(T1)
  type(dt(len=*)) t1(1)

  TYPE(DT(4,     LEN=T0(1)%I))  :: T2(1)!  =  DT(4, LEN=T0(1)%DT0%I)() 
  TYPE(DT(4,     L=T1%LEN))      :: T3(1)!  =  DT(4, L=T1%LEN)() 

  INTEGER :: J ! J is in common block A. However this was rejected. 336160
  EQUIVALENCE(J,I)
  COMMON /A/J  ! This was added after rejection

  TYPE(DT(4,        L=J))      :: T4(1)!  =  DT(4, L=J)() 

  IF ( T1%K               .NE.   4          ) STOP 11
  IF ( T1%L               .NE.   1          ) STOP 12
  IF ( T1%KIND            .NE.   4          ) STOP 13
  IF ( T1%LEN             .NE.   1          ) STOP 14
  IF ( ANY( T1%I          .NE.   4        ) ) STOP 15
  IF ( ANY( T1%R          .NE.   4        ) ) STOP 16
  IF ( ANY( T1%C          .NE.   CHAR(4)  ) ) STOP 17

  IF ( T2%K               .NE.   4          ) STOP 21
  IF ( T2%L               .NE.   1          ) STOP 22
  IF ( T2%KIND            .NE.   4          ) STOP 23
  IF ( T2%LEN             .NE.   4          ) STOP 24
  IF ( ANY( T2%I          .NE.   4        ) ) STOP 25
  IF ( ANY( T2%R          .NE.   4        ) ) STOP 26
  IF ( ANY( T2%C          .NE.   CHAR(4)  ) ) STOP 27

  IF ( T3%K               .NE.   4          ) STOP 31
  IF ( T3%L               .NE.   1          ) STOP 32
  IF ( T3%KIND            .NE.   4          ) STOP 33
  IF ( T3%LEN             .NE.   4          ) STOP 34
  IF ( ANY( T3%I          .NE.   4        ) ) STOP 35
  IF ( ANY( T3%R          .NE.   4        ) ) STOP 36
  IF ( ANY( T3%C          .NE.   CHAR(4)  ) ) STOP 37

  IF ( T4%K               .NE.   4          ) STOP 41
  IF ( T4%L               .NE.   1          ) STOP 42
  IF ( T4%KIND            .NE.   4          ) STOP 43
  IF ( T4%LEN             .NE.   4          ) STOP 44
  IF ( ANY( T4%I          .NE.   4        ) ) STOP 45
  IF ( ANY( T4%R          .NE.   4        ) ) STOP 46
  IF ( ANY( T4%C          .NE.   CHAR(4)  ) ) STOP 47

  END SUBROUTINE

  END

