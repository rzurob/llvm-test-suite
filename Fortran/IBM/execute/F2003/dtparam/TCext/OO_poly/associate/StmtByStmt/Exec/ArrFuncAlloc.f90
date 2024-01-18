! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrFuncAlloc.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ArrFuncAlloc.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ArrFuncAlloc
!*
!*  DATE                       : Nov. 02, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is a function returning an allocatable array
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS :: SetId
      PROCEDURE, PASS   :: GetObj
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
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

    ELEMENTAL FUNCTION GetObj(Arg)
    CLASS(Base(4)), INTENT(IN) :: Arg
    TYPE (Child(4))    :: GetObj
      SELECT TYPE (Arg)
      TYPE IS (Child(4))
        GetObj = Arg
      TYPE IS (Base(4))
        GetObj%Base = Arg
      CLASS DEFAULT
      END SELECT
    END FUNCTION

    SUBROUTINE SetId(Obj, Id)
    CLASS(Base(4))  :: Obj(:,:)
    INTEGER      :: Id
      SELECT TYPE (Obj)
      TYPE IS (Base(4))
        Obj%BaseId = Id
      TYPE IS (Child(4))
        Obj%ChildId = Id
      END SELECT
    END SUBROUTINE


    FUNCTION Fun(Arg)
    CLASS(Base(4)) :: Arg(2,*)
    CLASS(Base(4)), ALLOCATABLE :: Fun(:,:)
      ALLOCATE(Fun(SIZE(Arg,1), 2), SOURCE=Arg(:, 1:2))
    END FUNCTION

  END MODULE

  PROGRAM ArrFuncAlloc
  USE M
  IMPLICIT NONE
  INTEGER :: i

  ASSOCIATE ( As => Fun((/Base(4)(BaseId=-1), Base(4)(BaseId=-2),Base(4)(BaseId=-3), Base(4)(BaseId=-4) /)) )

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 32

    IF ( ANY (As%GetID()  .NE. RESHAPE((/-1,-2,-3,-4/), (/2,2/)) ) ) STOP 33
    IF ( ANY (As%BaseID   .NE. RESHAPE((/-1,-2,-3,-4/), (/2,2/)) ) ) STOP 34

    ASSOCIATE ( As1 => As%BaseId )
       IF ( ANY(As1 .NE. RESHAPE((/-1,-2,-3,-4/), (/2,2/)) ) ) STOP 42
    END ASSOCIATE

    ASSOCIATE (As =>  As%GetID())
      IF ( ANY(As .NE. RESHAPE((/-1,-2,-3,-4/), (/2,2/)) )) STOP 60
    END ASSOCIATE

    SELECT TYPE (As => As)
    CLASS IS ( Child(4))
      STOP 70
    CLASS DEFAULT
      STOP 80
    TYPE IS (Base(4))
    END SELECT

  END ASSOCIATE

  END
