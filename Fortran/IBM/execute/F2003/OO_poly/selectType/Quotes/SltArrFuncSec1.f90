! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltArrFuncSec1.f
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
!*  TEST CASE NAME             : SltArrFuncSec1
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 18, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type 
!*
!*  SECONDARY FUNCTIONS TESTED : Selector 
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*     
!*   The selector is  an array section from function call
!*  
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
    END TYPE 

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base), INTENT(INOUT) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base), INTENT(INOUT)  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrFuncSec1
  USE M
  IMPLICIT NONE
  TYPE(Child), TARGET :: V(10)
  INTEGER :: i
 
    DO i=1, 10
      V(i)%BaseId = i
      V(i)%ChildId = -i
    END DO


  SELECT TYPE ( V=>Fun(V, 2) )
    CLASS DEFAULT
      SELECT TYPE (V)
        TYPE IS (Child)
          IF ( ANY(LBOUND(V)     .NE. 1) ) STOP 30
          IF ( ANY(UBOUND(V)     .NE. 5) ) STOP 31

          IF ( ANY(V%Base%GetId() .NE. (/ 1, 3, 5, 7, 9/)) ) STOP 34
          IF ( ANY(V%GetId()      .NE. (/-1,-3,-5,-7,-9/)) ) STOP 35
          IF ( ANY(V%BaseId       .NE. (/ 1, 3, 5, 7, 9/)) ) STOP 36
          IF ( ANY(V%ChildId      .NE. (/-1,-3,-5,-7,-9/)) ) STOP 37

       CLASS DEFAULT
          STOP 40
        CLASS is (Child)
          STOP 56
        TYPE is (Base)
          STOP 57
        TYPE IS (Zero)
          STOP 58
      END SELECT

  END SELECT

  CONTAINS
  
  FUNCTION Fun(Arg1, Arg2)
  CLASS(*), TARGET :: Arg1(:)
  INTEGER          :: Arg2
  CLASS(*), POINTER :: Fun(:)
    Fun => Arg1(1::Arg2)
  END FUNCTION
 
 
  END



