! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltVarDummyUL.f
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
!*  TEST CASE NAME             : SltVarDummyUL
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
!*   The selector is an unlimited poly dummy 
!*    (ICE:297189)
!*    (Wrong result-300729)
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

    FUNCTION Ful(Arg)
    CLASS(Base), TARGET  :: Arg
    CLASS(*),    POINTER :: Ful
      Ful => Arg
    END FUNCTION

  END MODULE

  PROGRAM SltVarDummyUL
  USE M
  IMPLICIT  NONE

  INTERFACE
    SUBROUTINE Sub(Arg)
    IMPORT Ful 
      PROCEDURE (Ful) :: Arg
    END SUBROUTINE
  END INTERFACE

  CALL Sub(Ful)

  END  
 
  SUBROUTINE Sub(FArg)
  USE M
  PROCEDURE (Ful) :: Farg 

  TYPE (Child), TARGET :: Tar=Child(BaseId=-1, ChildId=-2)

  SELECT TYPE ( Ptr => FArg(Tar))
    CLASS DEFAULT
      STOP 20   
    CLASS is (Base)
      STOP 24
    TYPE is (INTEGER(1))
      STOP 24
    CLASS is (Child)
      STOP 25
    TYPE is (Child)
      IF ( Ptr%BaseId       .NE. -1 ) STOP 31 
      IF ( Ptr%ChildId      .NE. -2 ) STOP 32 
      IF ( Ptr%Base%GetId() .NE. -1 ) STOP 33 
      IF ( Ptr%GetId()      .NE. -2 ) STOP 34 
  END SELECT

  END SUBROUTINE
