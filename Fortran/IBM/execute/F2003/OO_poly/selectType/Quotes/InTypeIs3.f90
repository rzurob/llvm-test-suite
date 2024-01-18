! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 24, 2005
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
!*  Within TYPE IS, test intrinsic type with diff kind parameter
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM InTypeIs3
  IMPLICIT NONE
  INTEGER(4) :: V(3,3)=0

  V(1:3:2,1:3:2) = -1

  CALL Sub(V(1:3:2,1:3:2))

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg(2:3,3:4)

    SELECT TYPE (Arg=>Arg(2, 3))
    TYPE IS (INTEGER(1))
      STOP 22
    TYPE IS (INTEGER(2))
      STOP 23
    TYPE IS (INTEGER)
      IF ( Arg .NE. -1) ERROR STOP 21
    TYPE IS (INTEGER(8))
      STOP 24
    CLASS DEFAULT
      STOP 26
    END SELECT

    SELECT TYPE (Arg)
    TYPE IS (INTEGER(1))
      STOP 22
    TYPE IS (INTEGER(2))
      STOP 23
    TYPE IS (INTEGER)
      IF ( ANY(SHAPE(Arg) .NE. (/2,2/))) ERROR STOP 20
      IF ( SIZE(Arg) .NE.  4)  ERROR STOP 20
      IF ( Any(Arg   .NE. -1)) ERROR STOP 22
    TYPE IS (INTEGER(8))
      STOP 24
    CLASS DEFAULT
      STOP 41
    END SELECT

  END SUBROUTINE

  END



