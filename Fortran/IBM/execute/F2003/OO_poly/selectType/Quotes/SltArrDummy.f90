! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltArrDummy.f
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
!*  TEST CASE NAME             : SltArrDummy
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 06, 2005
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
!*   The selector is a dummy array 
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Zero
    END TYPE 

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      CLASS(Base), POINTER :: BasePtr => NULL()
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
    CLASS(Base)  :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base)  :: Arg(:)
      SELECT TYPE (Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrDummy
  USE M
  IMPLICIT NONE
  TYPE(Child), TARGET :: Arr(1000)

  CALL Sub( Arr, Arr, SIZE(Arr) )

  CONTAINS

  SUBROUTINE Sub(Arr0, Arr1, N)
  CLASS(Zero) :: Arr0(*), Arr1(*)
  INTEGER :: N 

  SELECT TYPE ( As0 => Arr0 )
    CLASS IS (Child)
      SELECT TYPE (As1 => Arr1 )
        TYPE IS (Child) 

          ! Can not quary size of assumed size array
          !  IF ( SIZE(As0)   .NE. SIZE(Arr1) ) STOP 42
          !  IF ( SIZE(As0)   .NE. SIZE(As1) ) STOP 42

          IF ( ANY(As0(:N)%Base%GetId() .NE. 1) ) STOP 34
          IF ( ANY(As0(:N)%GetId()      .NE. 2) ) STOP 35
          IF ( ANY(As0(:N)%BaseId       .NE. 1) ) STOP 36
          IF ( ANY(As0(:N)%ChildId      .NE. 2) ) STOP 37

          CALL As1(1)%SetId(As0(:N))
          CALL As1(1)%Base%SetId(As0(:N)%Base)

          IF ( ANY(As1(:N)%Base%GetId() .NE. -1 ) ) STOP 44
          IF ( ANY(As1(:N)%GetId()      .NE. -2 ) ) STOP 45
          IF ( ANY(As1(:N)%BaseId       .NE. -1 ) ) STOP 46
          IF ( ANY(As1(:N)%ChildId      .NE. -2 ) ) STOP 47

         CLASS DEFAULT
            STOP 40
      END SELECT

    TYPE is (Base)
      STOP 32
    TYPE IS (Zero)
      STOP 38

  END SELECT

  END SUBROUTINE

  END
  
