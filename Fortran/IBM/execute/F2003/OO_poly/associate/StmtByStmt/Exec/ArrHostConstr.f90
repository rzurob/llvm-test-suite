! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ArrHostConstr.f  
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
!*  TEST CASE NAME             : ArrHostConstr
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb 14, 2005
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
!*    The selector is a host associate name 
!*    Selector is an array constructor 
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
      private
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
    CLASS(Base) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base)  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM ArrHostConstr
  USE M
  IMPLICIT NONE
  INTEGER :: i

  CALL Sub((/(Child(ChildID=-2, BaseID=-1), i=1, 3)/) )

  CONTAINS

  SUBROUTINE Sub(Arr)
  CLASS(*) :: Arr(:)
 
  ASSOCIATE ( As => (/Arr, Arr(:)/) )
    IF (ANY(SHAPE(As) .NE. (/6/)))            STOP 32
  ASSOCIATE ( As => As(1::2) )
    SELECT TYPE ( As )
    CLASS IS (Child)
   
          IF (ANY(SHAPE(As) .NE. (/3/)))      STOP 33
          IF ( ANY(As%Base%GetId() .NE. -1) ) STOP 34
          IF ( ANY(As%GetId()      .NE. -2) ) STOP 35
          IF ( ANY(As%BaseId       .NE. -1) ) STOP 36
          IF ( ANY(As%ChildId      .NE. -2) ) STOP 37

          CALL As(1)%SetId(As)
          CALL As(1)%Base%SetId(As%Base)

          IF ( ANY(As%Base%GetId() .NE. 1 ) ) STOP 44
          IF ( ANY(As%GetId()      .NE. 2 ) ) STOP 45
          IF ( ANY(As%BaseId       .NE. 1 ) ) STOP 46
          IF ( ANY(As%ChildId      .NE. 2 ) ) STOP 47

    CLASS DEFAULT 
      STOP 38

  END SELECT

  END ASSOCIATE
  END ASSOCIATE

  END SUBROUTINE

  END
  


