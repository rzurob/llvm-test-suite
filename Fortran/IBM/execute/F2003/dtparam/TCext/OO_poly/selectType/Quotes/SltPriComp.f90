! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltPriComp.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltPriComp.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltPriComp
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
!*   The type spec is specified with a type with private components
!*    (297038)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND        :: K1
      INTEGER(K1), PRIVATE :: BaseId=-1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Base) :: Base1    ! (4)
      INTEGER(K1) :: Base1Id=1
    END TYPE

    TYPE, EXTENDS(Base1) :: Child    ! (4)
      PRIVATE
      TYPE(Base(K1))  :: Base1Comp
    END TYPE

    CONTAINS

    function genChild (id)
        integer, intent(in) :: id
        type(Child(4)) genChild

        genChild%base1comp%baseID = id
    end function

    FUNCTION GetBaseId(Arg)
    CLASS(Base(4))  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    integer function childCompID (c)
        type(child(4)), intent(in) :: c

        childCompID = c%Base1Comp%baseid
    end function

  END MODULE

  PROGRAM SltPriComp
  USE M
  IMPLICIT NONE

  CLASS(*) ,ALLOCATABLE :: Var

  ALLOCATE(Var, SOURCE=genChild(0))

  SELECT TYPE (Var)
   CLASS IS (Base(4))
     STOP 20
   TYPE IS (Base(4))
     STOP 21
   CLASS IS (Child(4))
     STOP 22
   TYPE IS (Child(4))
     IF (Var%GetId() .NE. -1 )      STOP 31
     IF (Var%Base1%Base1Id .NE. 1 ) STOP 32
     IF (Var%Base1Id .NE. 1 )       STOP 33
     if (childCompID(var) /= 0) stop 34
  END SELECT

  END
