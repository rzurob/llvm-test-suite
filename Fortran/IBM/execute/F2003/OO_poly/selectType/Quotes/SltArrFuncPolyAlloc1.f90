! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: SltArrFuncPolyAlloc1.f
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
!*  TEST CASE NAME             : SltArrFuncPolyAlloc1
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Jan. 18, 2005
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
!*   The selector is a poly allocatable array from an external function call
!*   forming an array section 
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
    END TYPE 

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1 
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER :: ChildId = 2 
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
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

  END MODULE


  PROGRAM SltArrFuncPolyAlloc
  USE M
  IMPLICIT NONE
  TYPE(Child), TARGET :: Tar

  INTERFACE
    FUNCTION Fun()
     IMPORT Child
     CLASS(Child), ALLOCATABLE :: Fun(:)
    END FUNCTION
  END INTERFACE
 

  SELECT TYPE ( As => Fun())
    CLASS IS (Child)
      SELECT TYPE (As => As(::2))
        CLASS DEFAULT
        SELECT TYPE ( As => As(::1) )
          TYPE IS (Child) 

            IF ( ANY(LBOUND(As) .NE. 1) )       STOP 41
            IF ( SIZE(As)   .NE. 5  )           STOP 42
            IF ( ANY(As%GetId()      .NE. 2 ) ) STOP 43
            IF ( ANY(As%Base%GetId() .NE. 1 ) ) STOP 44
            IF ( ANY(As%GetId() .NE. 2 ) ) STOP 54
            IF ( ANY(As%ChildId .NE. 2 ) ) STOP 55

         CLASS DEFAULT
            STOP 40
        END SELECT
      END SELECT

    CLASS DEFAULT
      STOP 32

  END SELECT

  END

  FUNCTION Fun()
  USE M
  CLASS(Child), ALLOCATABLE :: Fun(:)
    ALLOCATE(Child :: Fun(10))
  END FUNCTION
   


