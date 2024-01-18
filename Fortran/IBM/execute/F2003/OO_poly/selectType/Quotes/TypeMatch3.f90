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
!*   The nearest type matchs that of selector
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
    END TYPE

    TYPE, EXTENDS(Zero)  :: One
    END TYPE

    TYPE, EXTENDS(One) :: Two
    END TYPE

    TYPE, EXTENDS(Two) :: Three
    END TYPE

  END MODULE

  PROGRAM TypeMatch3
  USE M,  One => One
  IMPLICIT NONE
  CLASS(*), POINTER :: U(:,:)

    ALLOCATE(Two :: U(2:3,3:4) )

    SELECT TYPE (U)
    CLASS IS (Zero)
       STOP 40
    CLASS IS (Three)
       STOP 41
    TYPE IS (Complex)
       STOP 42
    TYPE IS (INTEGER(2))
       STOP 43
    TYPE IS (INTEGER(4))
       STOP 44
    TYPE IS (One)
       STOP 47
    CLASS IS (One)

      IF ( SIZE(U)          .NE. 4 )          STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   STOP 34

    TYPE IS (REAL)
       STOP 45
    CLASS DEFAULT
       STOP 46
    END SELECT

  END



