! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  FuncULPolyAlloc.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : FuncULPolyAlloc
!*
!*  DATE                       : Feb 14, 2005
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
!*    The selector is a function call returning an unlimited poly allocatable
!*
!*    (299787)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: Base
      INTEGER :: BaseID=1
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      TYPE(Base) :: BS=Base(0)
      CLASS(Base), POINTER :: BSPtr=>NULL()
      INTEGER :: ChildID=2
    END TYPE

  CONTAINS

    FUNCTION ReturnObj(Arg)
    TYPE (Child) :: Arg
    CLASS (Base), ALLOCATABLE  :: ReturnObj
      ALLOCATE(ReturnObj, SOURCE=Arg)
    END FUNCTION

  END MODULE

  PROGRAM FuncULPolyAlloc
  USE M
  IMPLICIT NONE
  INTEGER :: i
  TYPE(Child), TARGET :: V=Child(BaseID=-1, ChildID=-2)

  ASSOCIATE ( As => TRANSFER(ReturnObj(V), ReturnObj(V)) )

    SELECT TYPE (As)
    TYPE IS (Child)

      IF ( As%ChildID    .NE. -2  )         STOP 33
      IF ( As%BS%BaseID  .NE.  0  )         STOP 34
      IF ( As%BaseID     .NE. -1  )         STOP 35

      ASSOCIATE (As => As)
        IF ( ASSOCIATED(As%BSPtr) )         STOP 36
      END ASSOCIATE

    CLASS DEFAULT
      STOP 88
    END SELECT

  END ASSOCIATE

  END

