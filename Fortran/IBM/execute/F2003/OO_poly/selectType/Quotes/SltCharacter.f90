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
!*   The selector is of character
!*    (comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM SltCharacter
  IMPLICIT NONE

  CLASS(*),     POINTER :: Ptr(:)
  CHARACTER,     TARGET :: C(4)="1"
  CHARACTER(1),  TARGET :: C1(4)="2"
  CHARACTER(2),  TARGET :: C2(4)="12"

  Ptr => C

  SELECT TYPE ( As => Ptr )
  CLASS DEFAULT
    STOP 30
  TYPE IS (CHARACTER(*))
    IF ( ANY(SHAPE(As) .NE. (/4/))  )   ERROR STOP 31
    IF ( LBOUND(As, 1)    .NE. 1       )   ERROR STOP 32
    IF ( ANY(As        .NE. "1") )  ERROR STOP 32
  TYPE IS (REAL )
    STOP 34
  END SELECT

  Ptr => C1

  SELECT TYPE ( Ptr )
  CLASS DEFAULT
    STOP 40
  TYPE IS ( CHARACTER(*) )
    IF ( ANY(SHAPE(Ptr) .NE. (/4/))  )  ERROR STOP 42
    IF ( LBOUND(Ptr, 1) .NE. 1       )  ERROR STOP 43
    IF ( ANY( Ptr       .NE. "2")    )  ERROR STOP 44
  TYPE IS (DOUBLE PRECISION )
    STOP 45
  END SELECT

  Ptr => C2

  SELECT TYPE ( As => Ptr(::3) )
  CLASS DEFAULT
    STOP 50
  TYPE IS ( CHARACTER(  *) )
    IF ( ANY(SHAPE(As) .NE. (/2/))  )   ERROR STOP 51
    IF ( LBOUND(As, 1) .NE. 1       )   ERROR STOP 52
    IF ( ANY(As        .NE. "12") )     ERROR STOP 52
  TYPE IS (INTEGER)
    STOP 54
  END SELECT

  Ptr => C1

  SELECT TYPE ( As => Ptr(3:3) )
  CLASS DEFAULT
    SELECT TYPE ( As => As(1) )
    CLASS DEFAULT
      STOP 60
    TYPE IS ( CHARACTER(*))
      IF ( As   .NE. "2" )  ERROR STOP 62
    TYPE IS (BYTE     )
      STOP 64
    END SELECT
  END SELECT


  END

