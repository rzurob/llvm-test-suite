! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltFuncULPolyPtr.f
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
!*  TEST CASE NAME             : SltFuncULPolyPtr
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
!*   The selector is a poly func call returning a unlimited poly pointer entity.
!*    (ICE : 297363)
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


  PROGRAM SltFuncULPolyPtr
  USE M
  IMPLICIT  NONE

  INTERFACE
    FUNCTION Func(Arg)
      CLASS(*) :: Arg
      CLASS(*), POINTER :: Func 
    END FUNCTION 
  END INTERFACE


  INTEGER :: i
  !TYPE (Child), TARGET :: Arr(10) = (/(Child(BaseId=i, ChildId=-i), i = 1, 10) /)
  CLASS(*), POINTER :: Ptr
  TYPE (Child), TARGET :: Arr(10) 
  Arr = (/(Child(BaseId=i, ChildId=-i), i = 1, 10) /)

  Ptr => Arr(5)


  SELECT TYPE ( As => Func(Func(Func(Ptr))))
    CLASS DEFAULT
      STOP 30   
    TYPE is (CHARACTER(*))
      STOP 31 
    TYPE is (Base)
      STOP 32
    TYPE is (REAL)
      STOP 33 
    CLASS IS (Child)
      IF ( As%Base%GetId() .NE.  5 ) STOP 34 
      IF ( As%GetId()      .NE. -5 ) STOP 35
      IF ( As%BaseId       .NE.  5 ) STOP 36
      IF ( As%ChildId      .NE. -5 ) STOP 37
    CLASS IS (Zero)
      STOP 38 
  END SELECT


  END  

  FUNCTION Func(Arg)
  USE M
  CLASS(*) :: Arg
  CLASS(*), POINTER :: Func

  ALLOCATE(Func, SOURCE=IntF(Arg))

  CONTAINS

  FUNCTION IntF(Arg)
  CLASS(*) :: Arg
  CLASS(*), POINTER :: IntF
    aLLOCATE(IntF, SOURCE=Arg)
  END FUNCTION

  END FUNCTION
 
