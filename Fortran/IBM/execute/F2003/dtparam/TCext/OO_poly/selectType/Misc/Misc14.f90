! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/selectType/Misc/Misc14.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Misc14.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Misc14
!*
!*  DATE                       : Jan. 12, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED :
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
!*  Wrong result-select type/allocate/polyEntity
!*    (295710)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
      TYPE(Base(K1)) :: BaseComp = Base(K1)(0)
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child(4)) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base(4))  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM  Misc14
  USE M

  IMPLICIT NONE

  CLASS(Base(4)), ALLOCATABLE   :: U

    ALLOCATE(U, SOURCE=Child(4)(BaseId=-1, ChildId=-2, BaseComp=Base(4)(0)))

    SELECT TYPE ( U )
      TYPE IS ( Child(4) )
!       print*, U%BaseId ! should be -1
!       print*, U%ChildId ! should be -2
!       print*, U%BaseComp%BaseId ! should be 0
        IF (U%BaseId   .NE. -1) STOP 21
        IF (U%ChildId  .NE. -2) STOP 22
        IF (U%BaseComp%BaseId .NE. 0) STOP 20
    end select

  END



