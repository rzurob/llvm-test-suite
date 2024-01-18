! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_procptr/CrossFeatures2/PtrAssignImp5.f
! opt variations: -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: PtrAssignImp5.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : PtrAssignImp5.f
!*
!*  DATE                       : Mar. 27, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment
!*
!*  REFERENCE                  : Feature 289058
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*  If proc-target and proc-pointer-object are functions,
!*  they shall have the same type; corresponding type parameters
!*  shall either both be deferred or both have the same value.
!*
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetBaseId
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetChildId
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  FUNCTION ExtFun(Arg)
  CLASS(*), POINTER :: ExtFun(:)
  CLASS(*)          :: Arg(:)
    ALLOCATE(ExtFun(2:1+SIZE(Arg)), SOURCE=Arg)
  END FUNCTION

  PROGRAM PtrAssignImp5
  USE M
  IMPLICIT NONE

  INTERFACE
    FUNCTION ExtFun(Arg)
      CLASS(*), POINTER :: ExtFun(:)
      CLASS(*)          :: Arg(:)
    END FUNCTION
  END INTERFACE

  PROCEDURE(ExtFun),   POINTER :: ProcPtr
  TYPE (Child(4)) :: V(3333) = Child(4)(BaseID=-1, ChildID=-2)

  ProcPtr => ExtFun
  SELECT TYPE ( As => ProcPtr(V))
  TYPE Is (Child(4))

    IF ( ANY(SHAPE(As) .NE. (/3333/) ))        STOP 11
    IF ( ANY(LBOUND(As) .NE. (/2/)) )          STOP 12
    IF ( ANY(UBOUND(As) .NE. (/1+SIZE(V)/) ))  STOP 13

    IF ( ANY(As%GetBaseID()  .NE. -1) ) STOP 21
    IF ( ANY(As%GetChildID() .NE. -2) ) STOP 22
    IF ( ANY(As%Base%GetID() .NE. -1) ) STOP 23
    IF ( ANY(As%GetID()      .NE. -2) ) STOP 24

  CLASS DEFAULT
    STOP 33
  END SELECT

  END

