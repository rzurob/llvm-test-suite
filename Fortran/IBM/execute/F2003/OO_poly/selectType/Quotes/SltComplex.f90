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
!*   The selector is of complex
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM SltComplex
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr(:)
  COMPLEX(4),  TARGET :: C4(4) = (1.,-1.)
  COMPLEX(8),  TARGET :: C8(4) = (1.,-1.)
  COMPLEX(16), TARGET :: C16(4) = (1.,-1.)
  COMPLEX,     TARGET :: C(4)  = (1.,-1.)

  Ptr => C4

  SELECT TYPE ( As => Ptr )
  CLASS DEFAULT
    STOP 30
  TYPE IS (COMPLEX(8) )
    STOP 34
  TYPE IS (COMPLEX(4))
    IF ( ANY(SHAPE(As) .NE. (/4/))  )   ERROR STOP 31
    IF ( LBOUND(As, 1) .NE. 1       )   ERROR STOP 32
    IF ( ANY(As        .NE. (1.,-1.)))  ERROR STOP 32
  END SELECT

  Ptr => C8

  SELECT TYPE ( Ptr )
  CLASS DEFAULT
    STOP 40
  TYPE IS (COMPLEX  )
    STOP 41
  TYPE IS (COMPLEX(8) )
    IF ( ANY(SHAPE(Ptr) .NE. (/4/))  )   ERROR STOP 42
    IF ( LBOUND(Ptr, 1) .NE. 1       )   ERROR STOP 43
    IF ( ANY(Ptr        .NE. (1._8,-1._8)) )  ERROR STOP 44
  TYPE IS (COMPLEX(16) )
    STOP 45
  END SELECT

  Ptr => C16

  SELECT TYPE ( As => Ptr(::3) )
  TYPE IS (COMPLEX(8))
    STOP 54
  CLASS DEFAULT
    STOP 50
  TYPE IS (COMPLEX( 16) )
    IF ( ANY(SHAPE(As) .NE. (/2/))  )   ERROR STOP 51
    IF ( LBOUND(As, 1) .NE. 1       )   ERROR STOP 52
    IF ( ANY(As        .NE. (1._16,-1._16)) )  ERROR STOP 53
  END SELECT

  Ptr => C

  SELECT TYPE ( As => Ptr(3) )
  CLASS DEFAULT
    STOP 60
  TYPE IS (COMPLEX)
    IF ( As .NE. (1.,-1.) )  ERROR STOP 62
  TYPE IS (COMPLEX(8) )
    STOP 64
  END SELECT


  END

