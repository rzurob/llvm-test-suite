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
!*   Match with nested select type construct
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
      INTEGER :: Id = 0
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    END TYPE

  END MODULE

  PROGRAM TypeMatch2
  USE M
  IMPLICIT NONE
  CLASS(*), POINTER :: V(:,:)

    ALLOCATE(V(2:3,3:4), SOURCE=Child())

    SELECT TYPE (V)
    TYPE IS (INTEGER(8))
      STOP 43
    TYPE IS (INTEGER(2))
      STOP 43
    TYPE IS (INTEGER(4))
      STOP 44
    CLASS DEFAULT
      SELECT TYPE (V)
      CLASS IS (Zero)
      SELECT TYPE (V)
      CLASS IS (Base)

       SELECT TYPE (V)
       CLASS IS (Child)
         STOP 40
       CLASS IS (Base)
         STOP 41
       TYPE IS (Child)

         IF ( ANY(V%Id         .NE. 0))          ERROR STOP 30
         IF ( SIZE(V)          .NE. 4 )          ERROR STOP 31
         IF ( ANY (LBOUND(V)   .NE. (/2, 3/) ) ) ERROR STOP 32
         IF ( ANY (UBOUND(V)   .NE. (/3, 4/) ) ) ERROR STOP 33
         IF ( ANY(SHAPE(V)     .NE. (/2,2/)) )   ERROR STOP 34

       TYPE IS (Base)
         STOP 45
      CLASS DEFAULT
         STOP 46
      END SELECT

    END SELECT
    END SELECT
    END SELECT

  END



