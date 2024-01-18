! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ArrFunc.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:  
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ArrFuncSec
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 02, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is a function return  of array
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
      CHARACTER(1023) :: C="!"
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS :: SetId
      PROCEDURE, PASS   :: GetObj
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    ELEMENTAL FUNCTION GetObj(Arg)
    CLASS(Base), INTENT(IN) :: Arg
    TYPE (Child)    :: GetObj
      SELECT TYPE (Arg)
      TYPE IS (Child)
        GetObj = Arg
      TYPE IS (Base)
        GetObj%Base = Arg
      CLASS DEFAULT
      END SELECT
    END FUNCTION

    SUBROUTINE SetId(Obj, Id)
    CLASS(Base)  :: Obj(:,:)
    INTEGER      :: Id
      SELECT TYPE (Obj)
      TYPE IS (Base)
        Obj%BaseId = Id
      TYPE IS (Child)
        Obj%ChildId = Id
      END SELECT
    END SUBROUTINE

  END MODULE

  PROGRAM ArrFunc
  USE M
  IMPLICIT NONE
  INTEGER :: i
  CLASS(Base), ALLOCATABLE :: Arr(:,:)
  INTEGER(8) :: S1(2)=(/5,4/), S2(2)=(/1,1/)

  ALLOCATE(Arr(5, 5), SOURCE=Child(BaseID=-1, ChildID=-2))

  ASSOCIATE ( As => Arr(S1, S2) )

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 30
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 31
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 32
    IF ( ANY (As%GetID()  .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 33
    IF ( ANY (As%BaseID   .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 36
    IF ( LEN(As%C)        .NE. 1023)                   STOP 34
    IF ( TRIM(As(1,1)%C)  .NE. "!" )                   STOP 35
 
  SELECT TYPE (As => As(:,:))
  CLASS IS ( Child)

    IF ( ANY (LBOUND(As)  .NE. (/1,1/) ) )             STOP 40
    IF ( ANY (UBOUND(As)  .NE. (/2,2/) ) )             STOP 41
    IF ( ANY (SHAPE(As)   .NE. (/2,2/) ) )             STOP 42
    IF ( ANY (As%GetID()       .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 43
    IF ( ANY (As%Base%GetID()  .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 46
    IF ( ANY (As%ChildID       .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 47
    IF ( ANY (As%BaseID        .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 48
    IF ( LEN(As%C)        .NE. 1023)                   STOP 44
    IF ( TRIM(As(2,2)%C)  .NE. "!" )                   STOP 45

  CLASS DEFAULT
    STOP 99
  END SELECT
  END ASSOCIATE

  END


