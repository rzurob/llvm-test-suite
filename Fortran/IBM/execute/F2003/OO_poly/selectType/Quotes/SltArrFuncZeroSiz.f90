! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltArrFuncZeroSiz.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltArrFuncZeroSiz
!*
!*  DATE                       : Jan. 17, 2005
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
!*   The selector is a function call returing a poly array of zero
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


 MODULE M

    TYPE  :: Zero
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = 2
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = 1
    END FUNCTION

  END MODULE


  PROGRAM SltArrFuncZeroSiz
  USE M
  IMPLICIT NONE
  TYPE(Child) :: V(3,3)
  INTEGER :: B1(3)=(/1,2,3/)
  INTEGER :: B2(3)=(/1,2,3/)

  SELECT TYPE ( Arg => Fun(Arg=Fun(Arg=V)) )
  CLASS DEFAULT
  ASSOCIATE( As => Arg )

      SELECT TYPE (As )
        TYPE IS (Child)

          IF ( ANY (SHAPE(As) .NE. (/3,3/) )) STOP 20
          IF ( ANY (As%Base%GetId() .NE. 1 )) STOP 34
          IF ( ANY (As%GetId()      .NE. 2 )) STOP 35

       CLASS DEFAULT
          STOP 40
       TYPE is (Base)
          STOP 32
       TYPE IS (Zero)
          STOP 38
      END SELECT

  END ASSOCIATE
  END SELECT

  CONTAINS

  FUNCTION Fun(Arg)
  CLASS(*), TARGET  :: Arg(:,:)
  CLASS(*), ALLOCATABLE :: Fun(:,:)

  ALLOCATE(Fun(SIZE(Arg,1), SIZE(Arg,2)), SOURCE=Arg)

  END FUNCTION
  END



