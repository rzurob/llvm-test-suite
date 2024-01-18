! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltImplicit.f
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
!*  TEST CASE NAME             : SltImplicit
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
!*   The selector is a poly var of implicit type
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero
    END TYPE

    TYPE, EXTENDS(Zero) :: Base
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

    SUBROUTINE Conv(CArg1, Carg2) 
    IMPLICIT TYPE(Child)(C)
       CArg2 = CArg1
    END SUBROUTINE

  END MODULE

  PROGRAM SltImpilcit
  USE M
  IMPLICIT TYPE(Child)(C)

  CLASS(Child), POINTER :: Ptr 

  ALLOCATE(Ptr, SOURCE=C)
  SELECT TYPE (Ptr)
    TYPE IS (Child)
      Ptr%BaseId = -1
      Ptr%ChildId = -2
  END SELECT
 
  CALL Conv(C, Ptr)

  SELECT TYPE ( As => Ptr )
    CLASS DEFAULT
      STOP 20   
    CLASS is (Child)
      STOP 24
    TYPE IS (Child)
      IF ( As%BaseId       .NE. 1 ) STOP 31 
      IF ( As%ChildId      .NE. 2 ) STOP 32 
      IF ( As%Base%GetId() .NE. 1 ) STOP 33 
      IF ( As%GetId()      .NE. 2 ) STOP 34 

  END SELECT

  END
