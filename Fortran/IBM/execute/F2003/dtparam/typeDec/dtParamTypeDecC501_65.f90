!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : May 01, 2007
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
!*  -- pointer/allocatable in expr
!*
!*  (340600)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM dtParamTypeDecC501_65

  TYPE :: DT0(K, L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    INTEGER       :: I=K
  END TYPE

  TYPE, EXTENDS(DT0) :: DT(KIND, LEN)
    INTEGER(1), KIND     :: KIND=K
    INTEGER(2), LEN      :: LEN=K
    REAL(KIND)           :: R=KIND
    CHARACTER(LEN)       :: C=CHAR(KIND)
    TYPE(DT0(KIND, K)) :: T=DT0(KIND, K)()
  END TYPE

  TYPE(DT0(L=:)), POINTER :: Ptr
  TYPE(DT0(L=:)), ALLOCATABLE, TARGET :: Alloc

  ALLOCATE ( DT0(4, L=2) :: Alloc )
  Ptr => Alloc

  CALL IntSub(  )

  CONTAINS

  SUBROUTINE IntSub()

  TYPE(DT(4,     LEN=Ptr%I))   :: T2(1)!  =  DT(4, LEN=Ptr%I)()
  TYPE(DT(4,       L=Alloc%L)) :: T3(1)!  =  DT(4, L=Alloc%L)()


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
  IF ( T3%L               .NE.   2          ) STOP 32
  IF ( T3%KIND            .NE.   4          ) STOP 33
  IF ( T3%LEN             .NE.   4          ) STOP 34
  IF ( ANY( T3%I          .NE.   4        ) ) STOP 35
  IF ( ANY( T3%R          .NE.   4        ) ) STOP 36
  IF ( ANY( T3%C          .NE.   CHAR(4)  ) ) STOP 37
  IF ( T3%T%K             .NE.   4          ) STOP 38
  IF ( T3%T%L             .NE.   4          ) STOP 39

  END SUBROUTINE

  END

