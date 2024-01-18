! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltHostVar.f
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
!*  TEST CASE NAME             : SltHostVar
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 16, 2004
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
!*   The selector is an associate name associating to a poly var 
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero
    END TYPE 

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      CLASS(Base), POINTER :: BasePtr => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: SetId => SetChildId
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
    CLASS(Base)  :: Arg
      Arg%BaseId = -1
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child)  :: Arg
      Arg%ChildId = -2
    END SUBROUTINE

  END MODULE


  PROGRAM SltHostVar
  USE M
  IMPLICIT  NONE


  SELECT TYPE ( As => Fun() )
    CLASS DEFAULT
      SELECT TYPE ( As )
        CLASS DEFAULT
          SELECT TYPE ( As )
            CLASS DEFAULT
              SELECT TYPE ( As )
                CLASS DEFAULT
                  SELECT TYPE ( As )
                    CLASS DEFAULT
                      SELECT TYPE ( AS )
                        CLASS IS (Child)
                          CALL As%SetId()
                          CALL As%Base%SetId()
                          IF ( As%Base%GetId() .NE. -1 ) STOP 34
                          IF ( As%GetId()      .NE. -2 ) STOP 35
                          IF ( As%BaseId       .NE. -1 ) STOP 36
                          IF ( As%ChildId      .NE. -2 ) STOP 37
                        TYPE is (Base)
                          STOP 32
                        CLASS IS (Zero)
                          STOP 38
                      END SELECT
                  END SELECT
              END SELECT
          END SELECT
      END SELECT
  END SELECT


  CONTAINS

  FUNCTION Fun()
    CLASS(Zero), ALLOCATABLE :: Fun
    ALLOCATE(Child :: Fun)
  END FUNCTION

  END
  
