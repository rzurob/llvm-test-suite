! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltFuncPolyPtr.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

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
!*   The selector is a poly func call returning a pointer entity.
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
      CLASS(Base(K1,:)), POINTER :: BasePtr => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4,*))  :: Arg
      Arg%BaseId = -1
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4,*))  :: Arg
      Arg%ChildId = -2
    END SUBROUTINE

  END MODULE


  PROGRAM SltFuncPolyPtr
  USE M
  IMPLICIT  NONE

  INTERFACE
    FUNCTION Func(Arg)
      CLASS(*) :: Arg
      CLASS(*), POINTER :: Func
    END FUNCTION
  END INTERFACE


  SELECT TYPE ( As => Func(Zero(4,20)()))
    CLASS IS (Zero(4,*))
      STOP 20
    TYPE is (CHARACTER(*))
      STOP 21
    TYPE is (Base(4,*))
      IF ( As%GetId() .NE. 1 ) ERROR STOP 22
      IF ( As%BaseId  .NE. 1 ) ERROR STOP 23
    CLASS DEFAULT
      STOP 24
  END SELECT

  SELECT TYPE ( As => Func(Func(Func(Zero(4,20)()))))
    CLASS DEFAULT
      STOP 30
    TYPE is (CHARACTER(*))
      STOP 31
    TYPE is (Base(4,*))
      STOP 32
    TYPE is (REAL)
      STOP 33
    CLASS IS (Child(4,*))
      IF ( As%Base%GetId() .NE. 1 ) ERROR STOP 34
      IF ( As%GetId()      .NE. 2 ) ERROR STOP 35
      IF ( As%BaseId       .NE. 1 ) ERROR STOP 36
      IF ( As%ChildId      .NE. 2 ) ERROR STOP 37
    CLASS IS (Zero(4,*))
      STOP 38
  END SELECT

  SELECT TYPE ( As => Func(123))
    CLASS DEFAULT
      STOP 40
    TYPE is (CHARACTER(*))
      IF (As .NE. "UNKNOWN!" ) ERROR STOP  41
    TYPE is (Base(4,*))
      STOP 42
    CLASS IS (Base(4,*))
      STOP 43
  END SELECT



  END

  FUNCTION Func(Arg)
  USE M
  CLASS(*) :: Arg
  CLASS(*), POINTER :: Func

  SELECT TYPE (Arg)
    CLASS IS (Zero(4,*))
      ALLOCATE(Func, SOURCE=Base(4,20)())
    CLASS IS (Base(4,*))
      ALLOCATE(Func, SOURCE=Child(4,20)(Base=Arg))
    CLASS IS (Child(4,*))
      ALLOCATE(Func, SOURCE=Arg)
    CLASS DEFAULT
      ALLOCATE(Func, SOURCE="UNKNOWN!")
  END SELECT
  END FUNCTION
