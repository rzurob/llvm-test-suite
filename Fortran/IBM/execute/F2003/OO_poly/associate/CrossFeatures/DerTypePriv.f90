! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  DerTypePriv.f  
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
!*  TEST CASE NAME             : DerTypePriv
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 07, 2005
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
!*    The selector is of a derived type with private components 
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT, PRIVATE ::  Zero
    END TYPE

    TYPE, EXTENDS(Zero) :: Base
      INTEGER, PRIVATE :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId 
    END TYPE
 
    TYPE, EXTENDS(Base) :: Child
      INTEGER, PRIVATE  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    SUBROUTINE SetId(Arg1, Arg2)
    CLASS(Base)         :: Arg1
    INTEGER, INTENT(IN) :: Arg2  
      SELECT TYPE(Arg1)
      TYPE IS (Base)
        Arg1%BaseId = Arg2
      TYPE IS (Child)
        Arg1%ChildId = Arg2
      END SELECT
    END SUBROUTINE 

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

  END MODULE
  
  PROGRAM DerTypePriv
  USE M, DT2=>Child, DT1=>Base
  IMPLICIT NONE

  TYPE(DT2), TARGET :: T=DT2() 

  ASSOCIATE( As => T )
  ASSOCIATE( V => T )

    IF ( As%Base%GetID() .NE. 1 ) STOP 20 
    IF ( As%GetID()      .NE. 2 ) STOP 21 
     
    CALL As%Base%SetID(-1) 
    CALL As%SetID(-2) 

    IF ( V%Base%GetID() .NE. -1 ) STOP 30 
    IF ( V%GetID()      .NE. -2 ) STOP 31 
  END ASSOCIATE
  END ASSOCIATE

  IF ( T%Base%GetID() .NE. -1 ) STOP 40 
  IF ( T%GetID()      .NE. -2 ) STOP 41 

  END


