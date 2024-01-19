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
!*   The selector is of double precision
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM SltDbl
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr(:)
  DOUBLE PRECISION,  TARGET :: D(4) = (/1.,2.,3.,4./)

  Ptr => D

  SELECT TYPE ( As => Ptr )
  CLASS DEFAULT
    STOP 30
  TYPE IS (REAL(4) )
    STOP 34
  TYPE IS (REAL(8))
    IF ( ANY(SHAPE(As) .NE. (/4/))  )   ERROR STOP 31
    IF ( LBOUND(As, 1) .NE. 1       )   ERROR STOP 32
    IF ( ANY(As        .NE. (/1._4,2._4,3._4,4._4/)) )  ERROR STOP 32
  END SELECT

  SELECT TYPE ( Ptr )
  CLASS DEFAULT
    STOP 40
  TYPE IS (REAL  )
    STOP 41
  TYPE IS (DOUBLE PRECISION)
    IF ( ANY(SHAPE(Ptr) .NE. (/4/))  )   ERROR STOP 42
    IF ( LBOUND(Ptr, 1) .NE. 1       )   ERROR STOP 43
    IF ( ANY(Ptr        .NE. (/1._8,2._8,3._8,4._8/)) )  ERROR STOP 44
  TYPE IS (REAL(16) )
    STOP 45
  END SELECT

  SELECT TYPE ( As => Ptr(::3) )
  TYPE IS (REAL(4))
    STOP 54
  CLASS DEFAULT
    STOP 50
  TYPE IS (REAL( 8) )
    IF ( ANY(SHAPE(As) .NE. (/2/))  )   ERROR STOP 51
    IF ( LBOUND(As, 1) .NE. 1       )   ERROR STOP 52
    IF ( ANY(As        .NE. (/1.,4./)) )  ERROR STOP 53
  END SELECT

  SELECT TYPE ( As => Ptr(3) )
  CLASS DEFAULT
    STOP 60
  TYPE IS (DOUBLE PRECISION)
    IF ( As .NE. 3. )  ERROR STOP 62
  TYPE IS (REAL(16))
    STOP 64
  END SELECT


  END

