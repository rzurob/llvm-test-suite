!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dtParamTypeDecC501
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
!*  C501 (R501) In a declaration-type-spec, every type-param-value that is
!*  not a colon or an asterisk shall be a specification-expr
!*  -- A constant or subobject of a constant
!*  (340476)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC501

  TYPE :: DT0(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    INTEGER       :: I=K
  END TYPE

  TYPE, EXTENDS(DT0) :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=K
    INTEGER(2), LEN      :: LEN=K
    REAL(KIND)           :: R=KIND
    CHARACTER(LEN)       :: C=CHAR(KIND)
    TYPE(DT0(KIND, LEN)) :: T=DT0(KIND, 4)()
  END TYPE

  TYPE(DT(KIND=4,          LEN=4)), PARAMETER :: T1(1)  =  DT(KIND=4_1, LEN=4_8)()

  TYPE(DT(T1(1)%DT0%I,     LEN=T1(1)%DT0%I))  :: T2(1)  =  DT(T1(1)%DT0%I, LEN=T1(1)%DT0%I)()
  TYPE(DT(KIND=T1(1)%K,    L=T1%LEN))         :: T3(1)  =  DT(KIND=T1(1)%K, L=T1%LEN)()


  IF ( T1%K               .NE.   4          ) STOP 11
  IF ( T1%L               .NE.   1          ) STOP 12
  IF ( T1%KIND            .NE.   4          ) STOP 13
  IF ( T1%LEN             .NE.   4          ) STOP 14
  IF ( ANY( T1%I          .NE.   4        ) ) STOP 15
  IF ( ANY( T1%R          .NE.   4        ) ) STOP 16
  IF ( ANY( T1%C          .NE.   CHAR(4)  ) ) STOP 17
  IF ( T1%DT0%K           .NE.   4          ) STOP 18
  IF ( T1%DT0%L           .NE.   1          ) STOP 19

  IF ( T2%K               .NE.   4          ) STOP 21
  IF ( T2%L               .NE.   1          ) STOP 22
  IF ( T2%KIND            .NE.   4          ) STOP 23
  IF ( T2%LEN             .NE.   4          ) STOP 24
  IF ( ANY( T2%I          .NE.   4        ) ) STOP 25
  IF ( ANY( T2%R          .NE.   4        ) ) STOP 26
  IF ( ANY( T2%C          .NE.   CHAR(4)  ) ) STOP 27
  IF ( T2%DT0%K           .NE.   4          ) STOP 28
  IF ( T2%DT0%L           .NE.   1          ) STOP 29

  IF ( T3%K               .NE.   4          ) STOP 31
  IF ( T3%L               .NE.   4          ) STOP 32
  IF ( T3%KIND            .NE.   4          ) STOP 33
  IF ( T3%LEN             .NE.   4          ) STOP 34
  IF ( ANY( T3%I          .NE.   4        ) ) STOP 35
  IF ( ANY( T3%R          .NE.   4        ) ) STOP 36
  IF ( ANY( T3%C          .NE.   CHAR(4)  ) ) STOP 37
  IF ( T3%DT0%K           .NE.   4          ) STOP 38
  IF ( T3%DT0%L           .NE.   4          ) STOP 39

  END

