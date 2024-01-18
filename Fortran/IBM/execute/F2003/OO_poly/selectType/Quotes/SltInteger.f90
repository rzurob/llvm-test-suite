! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 13, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
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
!*
!*   The selector is of integer
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM SltInteger
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr(:)
  INTEGER(1), TARGET :: I1(4) = (/1,2,3,4/)
  INTEGER(2), TARGET :: I2(4) = (/1,2,3,4/)
  INTEGER(4), TARGET :: I4(4) = (/1,2,3,4/)
  INTEGER(8), TARGET :: I8(4) = (/1,2,3,4/)

  Ptr => I1

  SELECT TYPE ( As => Ptr )
  CLASS DEFAULT
    STOP 30
  TYPE IS (INTEGER )
    STOP 34
  TYPE IS (INTEGER(1))
    IF ( ANY(SHAPE(As) .NE. (/4/))  )   STOP 31
    IF ( LBOUND(As, 1) .NE. 1       )   STOP 32
    IF ( ANY(As        .NE. (/1_1,2_1,3_1,4_1/)) )  STOP 32
  END SELECT

  Ptr => I2

  SELECT TYPE ( Ptr )
  CLASS DEFAULT
    STOP 40
  TYPE IS (INTEGER(1)  )
    STOP 41
  TYPE IS (INTEGER(2) )
    IF ( ANY(SHAPE(Ptr) .NE. (/4/))  )   STOP 42
    IF ( LBOUND(Ptr, 1) .NE. 1       )   STOP 43
    IF ( ANY(Ptr        .NE. (/1_2,2_2,3_2,4_2/)) )  STOP 44
  TYPE IS (INTEGER )
    STOP 45
  END SELECT

  Ptr => I4

  SELECT TYPE ( As => Ptr(::3) )
  TYPE IS (INTEGER(2))
    STOP 54
  CLASS DEFAULT
    STOP 50
  TYPE IS (INTEGER( 4) )
    IF ( ANY(SHAPE(As) .NE. (/2/))  )   STOP 51
    IF ( LBOUND(As, 1) .NE. 1       )   STOP 52
    IF ( ANY(As        .NE. (/1,4/)) )  STOP 53
  END SELECT

  Ptr => I8

  SELECT TYPE ( As => Ptr(3) )
  CLASS DEFAULT
    STOP 60
  TYPE IS (INTEGER(8))
    IF ( As   .NE. 3 )  STOP 62
  TYPE IS (INTEGER )
    STOP 64
  END SELECT


  END

