! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 04, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*  Complain  :
!*  " The selector in the SELECT TYPE statement has a syntax error."
!*    (296497)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM  Misc10
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr

  ALLOCATE(Ptr, SOURCE=1_1 )

  SELECT TYPE ( As => (/Ptr/) )
    TYPE IS (INTEGER(1))
      IF ( ANY(AS .NE. 1_1 )) STOP 20
    CLASS DEFAULT
      STOP 30
  END SELECT

  END

