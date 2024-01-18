! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltAbsCom.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltAbsCom
!*
!*  DATE                       : Dec. 14, 2004
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
!*   The selector is specified with abstract component
!*    (Comp failed)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Child) :: Abs
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM SltAbsCom
  USE M
  IMPLICIT NONE

  CLASS(Base), POINTER :: Ptr(:)
  TYPE(Child), TARGET :: Tar(4)

  CLASS(Base), POINTER :: Ptr1
  TYPE(Child), TARGET :: Tar1

  Ptr1 => Tar1

  SELECT TYPE ( Ptr1 )
  CLASS DEFAULT
    STOP 20
  TYPE IS (Child)
    IF ( Ptr1%ChildId      .NE. 2 )  STOP 33
    IF ( Ptr1%GetId()      .NE. 2 )  STOP 33
    IF ( Ptr1%Base%BaseId  .NE. 1 )  STOP 34
!   IF ( Ptr1%Base%GetId() .NE. 1 )  STOP 34 C611
  END SELECT

  Ptr => Tar

  SELECT TYPE ( As => Ptr )
  CLASS DEFAULT
    STOP 40
  TYPE IS (Child)
    IF ( ANY(SHAPE(As) .NE. (/4/))  )   STOP 41
    IF ( LBOUND(As, 1) .NE. 1       )   STOP 42
    IF ( ANY(As%ChildId      .NE. (/2,2,2,2/)) )  STOP 43
    IF ( ANY(As%GetId()      .NE. (/2,2,2,2/)) )  STOP 43
    IF ( ANY(As%Base%BaseId  .NE. (/1,1,1,1/)) )  STOP 44
!   IF ( ANY(As%Base%GetId() .NE. (/1,1,1,1/)) )  STOP 44 !C611
  END SELECT

  END

