! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  HostAssocVarPolyDummy.f  
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
!*  TEST CASE NAME             : HostAssocVarPolyDummy
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
!*    The selector is an associate name associating to a poly dummy variable of derived types
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId 
    END TYPE

    CONTAINS

    FUNCTION GetChildId(Arg)
    CLASS(Child) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM HostAssocVarPolyDummy
  USE M
  IMPLICIT NONE
 
  CLASS(Base), ALLOCATABLE :: V
  CLASS(*),    ALLOCATABLE :: U

  ALLOCATE(V, SOURCE=Child(BaseId=-1, ChildId=-2))
  ALLOCATE(U, SOURCE=Child(BaseId=-1, ChildId=-2))
  CALL Sub(V, U)
  DEALLOCATE(V)
  DEALLOCATE(U) 
  CONTAINS

  SUBROUTINE Sub(Arg1, Arg2)
  IMPLICIT NONE
  CLASS(*) :: Arg1, Arg2  
 
  ASSOCIATE ( T1 => Arg1, T2 => Arg2 )
    SELECT TYPE ( T1 )
      TYPE IS ( Child )
        IF (T1%GetId()      .NE. -2 ) STOP 30
        IF (T1%Base%GetId() .NE. -1 ) STOP 31
        T1%BaseId  = 1
        T1%ChildId = 2
      CLASS DEFAULT
        STOP 32
    END SELECT

    SELECT TYPE ( T2 )
      TYPE IS ( Child )
        IF (T2%GetId()      .NE. -2 ) STOP 40
        IF (T2%Base%GetId() .NE. -1 ) STOP 41
        T2%BaseId  = 1
        T2%ChildId = 2
      CLASS DEFAULT
        STOP 42
    END SELECT

    ASSOCIATE ( As1 => T1, As2 => T2 )
      SELECT TYPE ( As1 )
        TYPE IS (Child )
          IF (As1%GetId()      .NE. 2 ) STOP 50
          IF (As1%Base%GetId() .NE. 1 ) STOP 51
        CLASS DEFAULT
          STOP 52
      END SELECT 

      SELECT TYPE ( As2 )
        TYPE IS (Child )
          IF (As2%GetId()      .NE. 2 ) STOP 60
          IF (As2%Base%GetId() .NE. 1 ) STOP 61
        CLASS DEFAULT
          STOP 62
      END SELECT 
    END ASSOCIATE

  END ASSOCIATE

  
  END SUBROUTINE

  END
