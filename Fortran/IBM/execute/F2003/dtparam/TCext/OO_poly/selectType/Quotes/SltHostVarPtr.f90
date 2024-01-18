! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltHostVarPtr.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 23, 2004
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
!*   The selector is an associate name associating to a pointer
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
      CLASS(Base(K1)), POINTER :: BasePtr => NULL()
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
      Arg%BaseId = -1
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4))  :: Arg
      Arg%ChildId = -2
    END SUBROUTINE

  END MODULE


  PROGRAM SltHostVarPtr
  USE M
  IMPLICIT  NONE
  CLASS(Zero(4)), POINTER :: Ptr
  CLASS(Child(4)), ALLOCATABLE, Target :: Tar

  ALLOCATE(Child(4) :: Tar)
  Ptr => Tar

  SELECT TYPE ( As => Ptr )
    CLASS IS (Zero(4))
      SELECT TYPE ( Ptr )
        CLASS IS (Zero(4))
          SELECT TYPE ( As )
            CLASS IS (Base(4))
              SELECT TYPE ( Ptr )
                CLASS IS (Base(4))
                  SELECT TYPE ( As )
                    CLASS IS (Child(4))
                      SELECT TYPE ( Ptr )
                        CLASS IS (Child(4))
                          IF ( As%Base%GetId() .NE.  1 ) ERROR STOP 34
                          IF ( As%GetId()      .NE.  2 ) ERROR STOP 35
                          IF ( As%BaseId       .NE.  1 ) ERROR STOP 36
                          IF ( As%ChildId      .NE.  2 ) ERROR STOP 37
                          CALL Ptr%SetId()
                          CALL Ptr%Base%SetId()
                          IF ( As%Base%GetId() .NE. -1 ) ERROR STOP 34
                          IF ( As%GetId()      .NE. -2 ) ERROR STOP 35
                          IF ( As%BaseId       .NE. -1 ) ERROR STOP 36
                          IF ( As%ChildId      .NE. -2 ) ERROR STOP 37
                      END SELECT
                  END SELECT
              END SELECT
          END SELECT
      END SELECT

    TYPE is (Base(4))
      STOP 32
    TYPE IS (Zero(4))
      STOP 38

  END SELECT


  END

