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
!*   The selector is of byte
!*    (Comp failed: 296997)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM SltByte
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr(:)
  BYTE, TARGET      :: BTar(4) = (/1, 2, 3, 4/)

  Ptr => BTar

  SELECT TYPE ( As => Ptr )
  CLASS DEFAULT
    STOP 30
  TYPE IS ( BYTE)
    IF ( ANY(SHAPE(As) .NE. (/4/))  )   ERROR STOP 31
    IF ( LBOUND(As ,1) .NE. 1       )   ERROR STOP 32
    IF ( ANY(As        .NE. (/1,2,3,4/)) )  ERROR STOP 32
  TYPE IS (CHARACTER(*) )
    STOP 34
  END SELECT

  SELECT TYPE ( As => Ptr(::3) )
  CLASS DEFAULT
    STOP 30
  TYPE IS (  BYTE )
    IF ( ANY(SHAPE(As) .NE. (/2/))  )   ERROR STOP 31
    IF ( LBOUND(As, 1) .NE. 1       )   ERROR STOP 32
    IF ( ANY(As        .NE. (/1,4/)) )  ERROR STOP 32
  TYPE IS (CHARACTER(*))
    STOP 34
  END SELECT

  SELECT TYPE ( As => Ptr(3) )
  CLASS DEFAULT
    STOP 40
  TYPE IS (     BYTE)
    IF ( As   .NE. 3 )  ERROR STOP 42
  TYPE IS (CHARACTER(*)            )
    STOP 34
  END SELECT


  END
