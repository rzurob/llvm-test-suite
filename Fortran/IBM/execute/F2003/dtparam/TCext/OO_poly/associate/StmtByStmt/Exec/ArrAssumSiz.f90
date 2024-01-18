! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrAssumSiz.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ArrAssumSiz.f  
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
!*  TEST CASE NAME             : ArrAssumSiz
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
!*    The selector an assumed size array 
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId 
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

  END MODULE

  PROGRAM ArrAssumSiz
  USE M
  IMPLICIT NONE
  INTEGER :: i
  TYPE (Child(4)) :: Arr(2, 2)=Child(4)(ChildId=-2, BaseId=-1)

  CALL Sub( Arr )

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg(3:4,2:*)  
   
  ASSOCIATE ( As => Arg )
  SELECT TYPE ( As )
  TYPE IS (Child(4))

    IF ( ANY (LBOUND(As)      .NE. (/3,2/) ) )             STOP 30
    IF ( ANY (SHAPE(As(:,2:3)).NE. (/2,2/) ) )             STOP 32
    IF ( ANY (As(:,2:3)%GetID()      .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 33 
    IF ( ANY (As(:,2:3)%Base%GetID() .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 34 

    ASSOCIATE ( As0 => As(:,2:3)%ChildId, As1 => As(:,2:3)%BaseId )
       IF ( ANY(As0 .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) ) ) STOP 41 
       IF ( ANY(As1 .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) ) ) STOP 42 
    END ASSOCIATE

    ASSOCIATE ( As2 => As(:,2:3)%Base )
      IF ( ANY(As2%GetID() .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) STOP 50
    END ASSOCIATE

    ASSOCIATE (As =>  As(3:,:3)%GetID())
      IF ( ANY(As .NE. RESHAPE((/-2,-2,-2,-2/), (/2,2/)) )) STOP 60 
    END ASSOCIATE

    ASSOCIATE (As =>  As(:4,2:3)%Base%GetID())
      IF ( ANY(As .NE. RESHAPE((/-1,-1,-1,-1/), (/2,2/)) )) STOP 70 
    END ASSOCIATE

    CALL As%SetId(As(:,:3)%Base, 1)
    CALL As%SetId(As(:,:3), 2)
    IF ( ANY (As(:,2:3)%GetID()      .NE. RESHAPE((/2,2,2,2/), (/2,2/)) ) ) STOP 83
    IF ( ANY (As(:,2:3)%Base%GetID() .NE. RESHAPE((/1,1,1,1/), (/2,2/)) ) ) STOP 84

  CLASS DEFAULT
    STOP 90
  END SELECT
  END ASSOCIATE

  END SUBROUTINE

  END
