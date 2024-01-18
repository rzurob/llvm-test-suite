! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: PtrAssignProcNameProcPtrcomp.f 
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
!*  TEST CASE NAME             : PtrAssignProcNameProcPtrComp.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 19, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : Pointer assignment 
!*
!*  REFERENCE                  : Feature 289058 
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
!*  C727 (R742) A procedure-name shall be the name of an external, module,
!*  or dummy procedure, a specific intrinsic function listed in 13.6
!*  and not marked with a bullet (.), or a procedure pointer.
!* 
!*  The target is a procedure pointer component 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
      INTEGER :: BaseId = 1
      PROCEDURE(IFun1), PASS, POINTER :: BasePtr=>NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
      PROCEDURE(IFun2), PASS, POINTER :: ChildPtr=>NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId 
    END TYPE

    INTERFACE 

      FUNCTION IFun1(Arg)
        IMPORT
        CLASS(Base)          :: Arg
        CLASS(Base), POINTER :: IFun1 
      END FUNCTION

      FUNCTION IFun2(Arg)
        IMPORT
        CLASS(Child)          :: Arg
        CLASS(Child), POINTER :: IFun2
      END FUNCTION

    END INTERFACE

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

  END MODULE

  FUNCTION ExtFun1(Arg)
  USE M
  CLASS(Base)          :: Arg
  CLASS(Base), POINTER :: ExtFun1 
      ALLOCATE(ExtFun1, SOURCE=Arg)
  END FUNCTION

  FUNCTION ExtFun2(Arg)
  USE M
  CLASS(Child)          :: Arg
  CLASS(Child), POINTER :: ExtFun2 
      ALLOCATE(ExtFun2, SOURCE=Arg)
  END FUNCTION


  PROGRAM PtrAssignProcNameProcPtrComp
  USE M
  IMPLICIT NONE

  TYPE (Child)   :: U, V
  TYPE (Base) :: VBase
 
  PROCEDURE(IFun1), POINTER :: ProcPtr1 
  PROCEDURE(IFun1)          :: ExtFun1
  PROCEDURE(IFun2), POINTER :: ProcPtr2 
  PROCEDURE(IFun2)          :: ExtFun2

  V = Child(BaseId=-1, ChildId=-2)  
  V%BasePtr  => ExtFun1
  V%ChildPtr => ExtFun2
 

  SELECT TYPE( As => V%Base%BasePtr())
  TYPE IS (Base)
    IF ( As%BaseId .NE. As%GetId() )             STOP 11
    IF ( .NOT. ASSOCIATED(As%BasePtr, ExtFun1) ) STOP 12
  CLASS DEFAULT
    STOP 13
  END SELECT 

  SELECT TYPE( As => V%ChildPtr())
  TYPE IS (Child)
    IF ( As%ChildId .NE. As%GetId() )             STOP 21
    IF ( .NOT. ASSOCIATED(As%ChildPtr, ExtFun2) ) STOP 22
  CLASS DEFAULT
    STOP 23
  END SELECT

  END

