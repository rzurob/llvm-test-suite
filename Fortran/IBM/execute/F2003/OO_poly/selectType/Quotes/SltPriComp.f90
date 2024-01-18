! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltPriComp.f
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
!*  TEST CASE NAME             : SltPriComp
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Dec. 14, 2004
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
!*   The type spec is specified with a type with private components 
!*    (297038)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base
      INTEGER, PRIVATE :: BaseId=-1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Base) :: Base1
      INTEGER :: Base1Id=1
    END TYPE

    TYPE, EXTENDS(Base1) :: Child
      PRIVATE
      TYPE(Base)  :: Base1Comp=Base(BaseId=0)
    END TYPE

    CONTAINS

    FUNCTION GetBaseId(Arg)
    CLASS(Base)  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM SltPriComp
  USE M
  IMPLICIT NONE

  CLASS(*) ,ALLOCATABLE :: Var

  ALLOCATE(Var, SOURCE=Child())

  SELECT TYPE (Var)
   CLASS IS (Base)
     STOP 20
   TYPE IS (Base)
     STOP 21
   CLASS IS (Child)
     STOP 22
   TYPE IS (Child)
     IF (Var%GetId() .NE. -1 )      STOP 31
     IF (Var%Base1%Base1Id .NE. 1 ) STOP 32
     IF (Var%Base1Id .NE. 1 )       STOP 33
  END SELECT

  END
