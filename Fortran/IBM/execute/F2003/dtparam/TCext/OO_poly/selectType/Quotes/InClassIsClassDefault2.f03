! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/InClassIsClassDefault2.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 25, 2005
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
!*  Within the CLASS DEFAULT and TYPE IS
!*  for extensible types
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890




  MODULE M

    TYPE  :: Zero(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base    ! (4)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4)) :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4))  :: Arg(:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM InClassIsClassDefault2
  USE M
  IMPLICIT NONE

  TYPE(Child(4)) :: Arr(2:3,3:4)

  CALL Sub(Arr)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg(2:3,3:4)

    SELECT TYPE (U => Arg)
    CLASS DEFAULT
    SELECT TYPE (U)
    CLASS IS (Child(4))
    SELECT TYPE (U)
    CLASS DEFAULT

      IF ( .NOT. SAME_TYPE_AS(U, Arg))       ERROR STOP 30
      IF ( SIZE(U)          .NE. 4 )          ERROR STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) ERROR STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) ERROR STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   ERROR STOP 34

      SELECT TYPE ( As => Arg )

      TYPE IS (Child(4))

          IF ( ANY(As%Base%GetId() .NE. U%Base%GetId()) ) ERROR STOP 34
          IF ( ANY(As%GetId()      .NE. U%GetId()) )      ERROR STOP 35
          IF ( ANY(As%BaseId       .NE. U%BaseId) )       ERROR STOP 36
          IF ( ANY(As%ChildId      .NE. U%ChildId) )      ERROR STOP 37

          CALL As(2,3)%SetId(As)
          CALL As(2,3)%Base%SetId(As%Base)

          IF ( ANY(As%Base%GetId() .NE. U%Base%GetId()) ) ERROR STOP 44
          IF ( ANY(As%GetId()      .NE. U%GetId()) )      ERROR STOP 45
          IF ( ANY(As%BaseId       .NE. U%BaseId) )       ERROR STOP 46
          IF ( ANY(As%ChildId      .NE. U%ChildId) )      ERROR STOP 47

      CLASS DEFAULT
        STOP 51
      END SELECT

    END SELECT
    END SELECT
    END SELECT

  END SUBROUTINE

  END


