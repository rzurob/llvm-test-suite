! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltFuncArgAlloc.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltFuncArgAlloc.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltFuncArgAlloc
!*
!*  DATE                       : Jan. 05, 2005
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
!*   The selector is a function return with associate name(to allocatable) as argument
!*    (297701)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero(K1)    ! (4)
        INTEGER, KIND :: K1
    CONTAINS
      PROCEDURE, NoPASS   :: ReturnObj
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

    FUNCTION ReturnObj(Arg)
    CLASS(Zero(4)) :: Arg
    CLASS(*), ALLOCATABLE :: ReturnObj
      ALLOCATE(ReturnObj, SOURCE=Arg)
    END FUNCTION

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


  PROGRAM SltFuncArgAlloc
  USE M
  IMPLICIT NONE
  CLASS(Zero(4)), ALLOCATABLE, TARGET :: V

  ALLOCATE(Child(4) :: V)

  SELECT TYPE ( As0 => V )
    CLASS IS (Zero(4))
      SELECT TYPE (As => As0%ReturnObj(As0))
        TYPE IS (Child(4))
          IF ( As%Base%GetId() .NE. 1 ) STOP 34
          IF ( As%GetId()      .NE. 2 ) STOP 35
          IF ( As%BaseId       .NE. 1 ) STOP 36
          IF ( As%ChildId      .NE. 2 ) STOP 37
          CALL As%SetId()
          CALL As%Base%SetId()
          IF ( As%Base%GetId() .NE. -1 ) STOP 34
          IF ( As%GetId()      .NE. -2 ) STOP 35
          IF ( As%BaseId       .NE. -1 ) STOP 36
          IF ( As%ChildId      .NE. -2 ) STOP 37
        CLASS DEFAULT
          STOP 40
      END SELECT

    TYPE is (Base(4))
      STOP 32
    TYPE IS (Zero(4))
      STOP 38
    CLASS DEFAULT
      STOP 39
  END SELECT

  END
