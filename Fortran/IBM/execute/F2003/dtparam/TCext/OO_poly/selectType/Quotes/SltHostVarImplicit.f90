! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltHostVarImplicit.f
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
!*   The selector is an associate name associating to an entity of implicit type
!*    (Ice : 297436 )
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
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4))  :: Arg
      Arg%ChildId = -Arg%ChildId
    END SUBROUTINE

  END MODULE


  PROGRAM SltHostVarImplicit
  USE M
  IMPLICIT CLASS(*)(V), TYPE(Zero(4))(B)
  ALLOCATABLE :: V

  ALLOCATE(V, SOURCE=Child(4)())

  SELECT TYPE ( As => V )
    CLASS IS (Child(4))
      IF ( As%Base%GetId() .NE.  1 ) ERROR STOP 34
      IF ( As%GetId()      .NE.  2 ) ERROR STOP 35
      IF ( As%BaseId       .NE.  1 ) ERROR STOP 36
      IF ( As%ChildId      .NE.  2 ) ERROR STOP 37
      CALL As%SetId()
      CALL As%Base%SetId()
      IF ( As%Base%GetId() .NE. -1 ) ERROR STOP 34
      IF ( As%GetId()      .NE. -2 ) ERROR STOP 35
      IF ( As%BaseId       .NE. -1 ) ERROR STOP 36
      IF ( As%ChildId      .NE. -2 ) ERROR STOP 37

    TYPE is (Base(4))
      STOP 32
    TYPE IS (Zero(4))
      STOP 38

  END SELECT


  END

