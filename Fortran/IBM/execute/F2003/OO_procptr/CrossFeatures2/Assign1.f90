! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  Assign1.f 
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
!*  TEST CASE NAME             : Assign1.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jun. 23, 2005
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Procedure pointer 
!*
!*  SECONDARY FUNCTIONS TESTED : 
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
!*  A derived-type intrinsic assignment 
!*  (ICE)  
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE :: Base
      PROCEDURE(ModFun1), PASS, POINTER :: ProcPtr1=>NULL()
    CONTAINS
      PROCEDURE, PASS :: Proc1 => ModFun1
    END TYPE

    TYPE, EXTENDS(BAse) :: DT
      PROCEDURE(ModFun2), PASS, POINTER :: ProcPtr2=>NULL()
    CONTAINS
      PROCEDURE, PASS :: Proc2 => ModFun2
    END TYPE

    INTERFACE ASSIGNMENT ( = )
      MODULE PROCEDURE PToP 
    END INTERFACE ASSIGNMENT ( = )

    CONTAINS

    FUNCTION ModFun1(Arg)
    CLASS(Base) :: Arg
    CLASS(Base), POINTER :: ModFun1
      ALLOCATE(ModFun1, SOURCE=Arg)
    END FUNCTION

    FUNCTION ModFun2(Arg)
    CLASS(DT) :: Arg
    CLASS(DT), POINTER :: ModFun2
      ALLOCATE(ModFun2, SOURCE=Arg)
    END FUNCTION

    SUBROUTINE PToP (Arg1, Arg2)
    TYPE(DT), INTENT (OUT) :: Arg1
    TYPE(Dt), INTENT (IN)  :: Arg2
      Arg1%Base = Arg2%Base  
      Arg1%ProcPtr2 => Arg2%ProcPtr2  
    END SUBROUTINE
 
  END MODULE


  PROGRAM Assign1 
  USE M
  IMPLICIT NONE 

  TYPE (DT)              :: V
  TYPE (Base)            :: V1
  TYPE (DT), ALLOCATABLE :: V2
  TYPE (Base), POINTER   :: V3

  PROCEDURE(ModFun1), POINTER :: ProcPtr1
  PROCEDURE(ModFun2), POINTER :: ProcPtr2

  ProcPtr1 => ModFun1 
  ProcPtr2 => ModFun2
 
  V = DT(Base=Base(ProcPtr1), ProcPtr2=ModFun2 )
  IF (.NOT. Equal(V, DT(Base=Base(ProcPtr1), ProcPtr2=ModFun2 )) ) STOP 11

  V1 = Base(ModFun1)
  IF (.NOT. Equal(V1, Base(ProcPtr1)) ) STOP 12

  ALLOCATE(V2, SOURCE=DT(Base=Base(ProcPtr1), ProcPtr2=ModFun2 ))
  IF (.NOT. Equal(V2, DT(Base=Base(ProcPtr1), ProcPtr2=ModFun2 )) ) STOP 13

  ALLOCATE(V3, SOURCE=Base(ProcPtr1) )
  IF (.NOT. Equal(V3, Base(ProcPtr1)) ) STOP 14


  CONTAINS

  FUNCTION Equal(Arg1, Arg2)
  LOGICAL Equal
  CLASS(Base) :: Arg1, Arg2
  
  Equal = .FALSE.

  SELECT TYPE ( Arg1 )
  TYPE IS (Base)
    SELECT TYPE ( Arg2 )
    TYPE IS (Base )
      Equal = ASSOCIATED(Arg1%ProcPtr1, Arg2%ProcPtr1) 
    CLASS DEFAULT
      STOP 22
    END SELECT
  TYPE IS (DT)  
    SELECT TYPE ( Arg2 )
    TYPE IS (DT )
      Equal = ASSOCIATED(Arg1%ProcPtr1, Arg2%ProcPtr1) 
      Equal = Equal .AND. ASSOCIATED(Arg1%ProcPtr2, Arg2%ProcPtr2) 
    CLASS DEFAULT
      STOP 33
    END SELECT
  CLASS DEFAULT
    STOP 44
  END SELECT

  END FUNCTION

  END

