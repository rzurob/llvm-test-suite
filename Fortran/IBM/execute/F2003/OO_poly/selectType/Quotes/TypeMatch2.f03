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
!*   Types specified for TYPE IS and CLASS IS are the same
!*   including intrinsic types
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    END TYPE

  END MODULE

  PROGRAM TypeMatch2
  USE M, Dt => Child
  IMPLICIT NONE
  CLASS(*), POINTER :: V(:,:)

    ALLOCATE(V(2:3,3:4), SOURCE=8_8)

    SELECT TYPE (U=>V)
    CLASS IS (Dt)
       STOP 40
    CLASS IS (Base)
       STOP 41
    TYPE IS (Dt)
       STOP 42
    TYPE IS (INTEGER(2))
       STOP 43
    TYPE IS (INTEGER(4))
       STOP 44
    TYPE IS (INTEGER(8))
      IF (ANY(U .NE. 8_8)) ERROR STOP 30

      IF ( SIZE(U)          .NE. 4 )          ERROR STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) ERROR STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) ERROR STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   ERROR STOP 34

    TYPE IS (Base)
       STOP 45
    CLASS DEFAULT
       STOP 46
    END SELECT

  END



