! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all /tstdev/OO_poly/associate/StmtByStmt/Exec/FuncArgAlloc.f
! *********************************************************************
!*  ===================================================================
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
!*    The associating entity associating to an allocatable is used as actual argument
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Foundation(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Foundation) :: Base    ! (4,20)
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
      TYPE(Foundation(K1,N1)), ALLOCATABLE :: FdComp
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

  END MODULE

  PROGRAM FuncArgAlloc
  USE M
  TYPE(Child(4,20)), ALLOCATABLE  :: V

  ALLOCATE(V)

  ASSOCIATE ( As => V  )
    ALLOCATE(As%FdComp, SOURCE=Foundation(4,20)())
    PRINT*, Func(As)
    IF ( As.ChildId .NE. -2 ) STOP 22
  END ASSOCIATE

  CONTAINS

  FUNCTION Func(Arg)
    CLASS(Base(4,*))              :: Arg
    CHARACTER(3)  :: Func

    SELECT TYPE (Arg)
      TYPE IS (Child(4,*))
        Arg%ChildId = -2
      CLASS DEFAULT
        STOP 11
    END SELECT
    Func = "OK!"

  END FUNCTION

  END
