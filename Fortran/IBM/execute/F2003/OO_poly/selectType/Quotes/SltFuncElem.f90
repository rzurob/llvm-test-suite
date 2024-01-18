! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltFuncElem.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltFuncElem
!*
!*  DATE                       : Dec. 16, 2004
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
!*   The selector is a poly elemental func call.
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
 !    CLASS(Base), POINTER :: BasePtr => NULL()   ! due to C1272
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: SetId => SetChildId
    END TYPE

    INTERFACE ASSIGNMENT (=)
      ELEMENTAL SUBROUTINE ElemS(Arg1, Arg2)
        CLASS(*), INTENT(INOUT) :: Arg1
        CLASS(*), INTENT(IN)    :: Arg2
      END SUBROUTINE
    END INTERFACE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base)  :: Arg
      Arg%BaseId = -1
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child)  :: Arg
      Arg%ChildId = -2
    END SUBROUTINE

  END MODULE


  PROGRAM SltFuncElem
  USE M
  IMPLICIT  NONE

  !CLASS(*), ALLOCATABLE :: V1(:)
  CLASS(*), POINTER :: V1(:)
  TYPE(Child) :: V2(5)
  INTEGER :: i

  ALLOCATE(Child :: V1(5))
  V2 = (/(Child(BaseID=i, ChildId=-i), i=1,5)/)

  SELECT TYPE ( V1 )
    CLASS DEFAULT
      STOP 30
    TYPE is (Base)
      STOP 32
    CLASS IS (Child)
      V1 = V2
      DO i=1, 5
        IF ( V1(i)%Base%GetId() .NE.  i ) STOP 34
        IF ( V1(i)%GetId()      .NE. -i ) STOP 35
        IF ( V1(i)%BaseId       .NE.  i ) STOP 36
        IF ( V1(i)%ChildId      .NE. -i ) STOP 37
      END DO
    CLASS IS (Zero)
      STOP 38
  END SELECT

  END

    ELEMENTAL SUBROUTINE ElemS(Arg1, Arg2)
    USE M, ONLY : Child
    IMPLICIT NONE
    CLASS(*), INTENT(INOUT) :: Arg1
    CLASS(*), INTENT(IN)    :: Arg2

      SELECT TYPE ( Arg1 )
        TYPE IS (Child)
          SELECT TYPE ( Arg2 )
            CLASS IS (Child)
              Arg1 = Arg2
          END SELECT
      END SELECT
    END SUBROUTINE


