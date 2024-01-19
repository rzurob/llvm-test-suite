! GB DTP extension using:
! ftcx_dtp -qk -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltFuncULPolyPtr.f
! opt variations: -qnok -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
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
!*   The selector is a poly func call returning a unlimited poly pointer entity.
!*    (ICE : 297363)
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


  PROGRAM SltFuncULPolyPtr
  USE M
  IMPLICIT  NONE

  INTERFACE
    FUNCTION Func(Arg)
      CLASS(*) :: Arg
      CLASS(*), POINTER :: Func
    END FUNCTION
  END INTERFACE


  INTEGER :: i
  !TYPE (Child), TARGET :: Arr(10) = (/(Child(BaseId=i, ChildId=-i), i = 1, 10) /)
  CLASS(*), POINTER :: Ptr
  TYPE (Child(4)), TARGET :: Arr(10)
  Arr = (/(Child(4)(BaseId=i, ChildId=-i), i = 1, 10) /)

  Ptr => Arr(5)


  SELECT TYPE ( As => Func(Func(Func(Ptr))))
    CLASS DEFAULT
      STOP 30
    TYPE is (CHARACTER(*))
      STOP 31
    TYPE is (Base(4))
      STOP 32
    TYPE is (REAL)
      STOP 33
    CLASS IS (Child(4))
      IF ( As%Base%GetId() .NE.  5 ) ERROR STOP 34
      IF ( As%GetId()      .NE. -5 ) ERROR STOP 35
      IF ( As%BaseId       .NE.  5 ) ERROR STOP 36
      IF ( As%ChildId      .NE. -5 ) ERROR STOP 37
    CLASS IS (Zero(4))
      STOP 38
  END SELECT


  END

  FUNCTION Func(Arg)
  USE M
  CLASS(*) :: Arg
  CLASS(*), POINTER :: Func

  ALLOCATE(Func, SOURCE=IntF(Arg))

  CONTAINS

  FUNCTION IntF(Arg)
  CLASS(*) :: Arg
  CLASS(*), POINTER :: IntF
    aLLOCATE(IntF, SOURCE=Arg)
  END FUNCTION

  END FUNCTION

