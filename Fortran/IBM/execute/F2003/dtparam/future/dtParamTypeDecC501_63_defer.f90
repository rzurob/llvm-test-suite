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
!*  -- type parameter inquiry
!*
!*  (similar to 336280/340567/345634)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM dtParamTypeDecC501_63

  TYPE :: DT0(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    INTEGER(K)    :: I=K
    CHARACTER(L)  :: C
  END TYPE

  TYPE(DT0(L=6)), PARAMETER :: T(2:3)=DT0(L=6)(C=CHAR(0))

  CALL IntSub()

  CONTAINS

  SUBROUTINE IntSub()

  TYPE, EXTENDS(DT0) :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=K
    INTEGER(2), LEN      :: LEN=K

    TYPE(DT0(K=4,       L=L))          :: T1(1)
    TYPE(DT0(K=4,       L=K ))         :: T2(1)
    TYPE(DT0(K=T%K,     L=T%K ))       :: T3(1)
    TYPE(DT0(K=4,       L=T%L ))       :: T4(1)
    TYPE(DT0(K=4,       L=T%I%KIND ))  :: T5(1)
    TYPE(DT0(K=4,       L=T%C%LEN ))   :: T6(1)
  END TYPE

  TYPE (DT(L=8)) :: T1

  IF ( T1%K               .NE.   4          ) ERROR STOP 11
  IF ( T1%L               .NE.   8          ) ERROR STOP 12
  IF ( T1%KIND            .NE.   4          ) ERROR STOP 13
  IF ( T1%LEN             .NE.   4          ) ERROR STOP 14
  IF ( T1%I               .NE.   4          ) ERROR STOP 15

  IF ( T1%T1%K            .NE.   4          ) ERROR STOP 20
  IF ( T1%T1%L            .NE.   8          ) ERROR STOP 21

  IF ( T1%T2%K            .NE.   4          ) ERROR STOP 30
  IF ( T1%T2%L            .NE.   4          ) ERROR STOP 31

  IF ( T1%T3%K            .NE.   4          ) ERROR STOP 40
  IF ( T1%T3%L            .NE.   4          ) ERROR STOP 21

  IF ( T1%T4%K            .NE.   4          ) ERROR STOP 50
  IF ( T1%T4%L            .NE.   6          ) ERROR STOP 51

  IF ( T1%T5%K            .NE.   4          ) ERROR STOP 60
  IF ( T1%T5%L            .NE.   4          ) ERROR STOP 61

  IF ( T1%T6%K            .NE.   4          ) ERROR STOP 70
  IF ( T1%T6%L            .NE.   6          ) ERROR STOP 71

  END SUBROUTINE

  END

