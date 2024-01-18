! GB DTP extension using:
! ftcx_dtp -qnol -qreuse=base /tstdev/OO_procptr/CrossFeatures2/PtrAssignProcNameProcPtrComp.f
! opt variations: -ql -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Mar. 19, 2005
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
!*  C727 (R742) A procedure-name shall be the name of an external, module,
!*  or dummy procedure, a specific intrinsic function listed in 13.6
!*  and not marked with a bullet (.), or a procedure pointer.
!*
!*  The target is a procedure pointer component
!*  ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
      PROCEDURE(IFun1), PASS, POINTER :: BasePtr=>NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
      PROCEDURE(IFun2), PASS, POINTER :: ChildPtr=>NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    INTERFACE

      FUNCTION IFun1(Arg)
        IMPORT
        CLASS(Base(4))          :: Arg
        CLASS(Base(4)), POINTER :: IFun1
      END FUNCTION

      FUNCTION IFun2(Arg)
        IMPORT
        CLASS(Child(4))          :: Arg
        CLASS(Child(4)), POINTER :: IFun2
      END FUNCTION

    END INTERFACE

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

  FUNCTION ExtFun1(Arg)
  USE M
  CLASS(Base(4))          :: Arg
  CLASS(Base(4)), POINTER :: ExtFun1
      ALLOCATE(ExtFun1, SOURCE=Arg)
  END FUNCTION

  FUNCTION ExtFun2(Arg)
  USE M
  CLASS(Child(4))          :: Arg
  CLASS(Child(4)), POINTER :: ExtFun2
      ALLOCATE(ExtFun2, SOURCE=Arg)
  END FUNCTION


  PROGRAM PtrAssignProcNameProcPtrComp
  USE M
  IMPLICIT NONE

  TYPE (Child(4))   :: U, V
  TYPE (Base(4)) :: VBase

  PROCEDURE(IFun1), POINTER :: ProcPtr1
  PROCEDURE(IFun1)          :: ExtFun1
  PROCEDURE(IFun2), POINTER :: ProcPtr2
  PROCEDURE(IFun2)          :: ExtFun2

  V = Child(4)(BaseId=-1, ChildId=-2)
  V%BasePtr  => ExtFun1
  V%ChildPtr => ExtFun2


  SELECT TYPE( As => V%Base%BasePtr())
  TYPE IS (Base(4))
    IF ( As%BaseId .NE. As%GetId() )             STOP 11
    IF ( .NOT. ASSOCIATED(As%BasePtr, ExtFun1) ) STOP 12
  CLASS DEFAULT
    STOP 13
  END SELECT

  SELECT TYPE( As => V%ChildPtr())
  TYPE IS (Child(4))
    IF ( As%ChildId .NE. As%GetId() )             STOP 21
    IF ( .NOT. ASSOCIATED(As%ChildPtr, ExtFun2) ) STOP 22
  CLASS DEFAULT
    STOP 23
  END SELECT

  END

