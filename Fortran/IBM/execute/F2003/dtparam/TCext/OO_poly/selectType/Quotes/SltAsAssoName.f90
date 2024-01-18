! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltAsAssoName.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 06, 2005
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
!*   The selector name is the same as its associate name
!*    ()
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
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: SetId => SetChildId
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
    CLASS(Base(4))  :: Arg
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4))  :: Arg
      Arg%ChildId = -Arg%ChildId
    END SUBROUTINE

  END MODULE


  PROGRAM SltAsAssoName
  USE M
  IMPLICIT NONE
  TYPE(Child(4)), TARGET :: Tar
  CLASS(*), POINTER  :: Ptr

  Ptr => Tar
  SELECT TYPE( Ptr => Ptr)
    CLASS IS (Child(4))
      SELECT TYPE( Ptr => Ptr )
        TYPE IS (Child(4))
          IF ( Ptr%Base%GetId() .NE. 1 ) STOP 34
          IF ( Ptr%GetId()      .NE. 2 ) STOP 35
          IF ( Ptr%BaseId       .NE. 1 ) STOP 36
          IF ( Ptr%ChildId      .NE. 2 ) STOP 37

          CALL Ptr%SetId()
          CALL Ptr%Base%SetId()

          IF ( Ptr%Base%GetId() .NE. -1 ) STOP 44
          IF ( Ptr%GetId()      .NE. -2 ) STOP 45
          IF ( Ptr%BaseId       .NE. -1 ) STOP 46
          IF ( Ptr%ChildId      .NE. -2 ) STOP 47
    END SELECT
  END SELECT
  END

