! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ArrZeroSiz.f  
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
!*  TEST CASE NAME             : ArrZeroSiz
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
!*    The selector an array of zero size
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId 
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

  PROGRAM ArrZeroSiz
  USE M
  IMPLICIT NONE
  INTEGER :: i
  TYPE (Child) :: Arr(10:0, 0:1)=Child(ChildId=-2, BaseId=-1)

   
  ASSOCIATE ( As => Arr )

    IF ( ANY (LBOUND(As)      .NE. (/1,0/) ) )             STOP 30
    IF ( ANY (SHAPE(As).NE. (/0,2/) ) )             STOP 32
 
    ! As is zerosized array. No comparison happens
    IF ( ANY (As%GetID()      .NE. RESHAPE((/-1,-2,-3,-4/), (/0,2/)) ) ) STOP 33 
    IF ( ANY (As%Base%GetID() .NE. RESHAPE((/-4,-3,-2,-1/), (/0,2/)) ) ) STOP 34 

    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( ANY(As0 .NE. RESHAPE((/-2,-2,-2,-2/), (/0,2/)) ) ) STOP 41 
       IF ( ANY(As1 .NE. RESHAPE((/-1,-1,-1,-1/), (/0,2/)) ) ) STOP 42 
    END ASSOCIATE

    ASSOCIATE ( As2 => As%Base )
      IF ( ANY(As2%GetID() .NE. RESHAPE((/-1,-1,-1,-1/), (/0,2/)) )) STOP 50
    END ASSOCIATE

    ASSOCIATE (As =>  As%GetID())
      IF ( ANY(As .NE. RESHAPE((/-2,-2,-2,-2/), (/0,2/)) )) STOP 60 
    END ASSOCIATE

    ASSOCIATE (As =>  As%Base%GetID())
      IF ( ANY(As .NE. RESHAPE((/-1,-1,-1,-1/), (/0,2/)) )) STOP 70 
    END ASSOCIATE

    !no calling happens
    CALL As%SetId(As%Base, 1)
    CALL As%SetId(As, 2)

    IF ( ANY (As%GetID()      .NE. RESHAPE((/2,2,2,2/), (/0,2/)) ) ) STOP 83
    IF ( ANY (As%Base%GetID() .NE. RESHAPE((/1,1,1,1/), (/0,2/)) ) ) STOP 84

  END ASSOCIATE

  END
