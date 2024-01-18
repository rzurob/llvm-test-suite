!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Apr. 24, 2007
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
!*  -- An object designator with a base object that is in a common block
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM dtParamTypeDecC501_2

  TYPE :: DT(K,L)
    INTEGER, KIND :: K=4
    INTEGER, LEN  :: L=1
    SEQUENCE
    INTEGER       :: I=K
  END TYPE


  TYPE(DT(K=4_1, L=1_8)) :: T(1)
  COMMON /A/T

  T = [DT(K=4_1, L=1_8)()]

  CALL IntSub(  )

  CONTAINS

  SUBROUTINE IntSub()
  TYPE(DT(K=4,          L=1)) :: T1(1)
  COMMON /A/T1

  TYPE(DT(4,     L=T1(1)%I))  :: T2(1)  !=  DT(4, L=4)()
  TYPE(DT(4,     L=T1%L))     :: T3(1)  !=  DT(4, L=1)()


  IF ( T1%K               .NE.   4          ) STOP 11
  IF ( T1%L               .NE.   1          ) STOP 12
  IF ( ANY( T1%I          .NE.   4        ) ) STOP 15

  IF ( T2%K               .NE.   4          ) STOP 21
  IF ( T2%L               .NE.   4          ) STOP 22
  IF ( ANY( T2%I          .NE.   4        ) ) STOP 25

  IF ( T3%K               .NE.   4          ) STOP 31
  IF ( T3%L               .NE.   1          ) STOP 32
  IF ( ANY( T3%I          .NE.   4        ) ) STOP 35

  END SUBROUTINE

  END

