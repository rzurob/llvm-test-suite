! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltArrFuncPolyAlloc.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 18, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   The selector is a poly allocatable array from an external function call
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1) :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE


  PROGRAM SltArrFuncPolyAlloc
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20)), TARGET :: Tar

  INTERFACE
    FUNCTION Fun()
     IMPORT Child
     CLASS(Child(4,:)), ALLOCATABLE :: Fun(:)
    END FUNCTION
  END INTERFACE


  SELECT TYPE ( As => Fun())
    CLASS IS (Child(4,*))
      SELECT TYPE (As => As)
        CLASS DEFAULT
        SELECT TYPE ( As )
          TYPE IS (Child(4,*))

            IF ( ANY(LBOUND(As) .NE. 1) )       ERROR STOP 41
            IF ( SIZE(As)   .NE. 10 )           ERROR STOP 42
            IF ( ANY(As%GetId()      .NE. 2 ) ) ERROR STOP 43
            IF ( ANY(As%Base%GetId() .NE. 1 ) ) ERROR STOP 44
            IF ( ANY(As%GetId() .NE. 2 ) ) ERROR STOP 54
            IF ( ANY(As%ChildId .NE. 2 ) ) ERROR STOP 55

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
  CLASS(Child(4,:)), ALLOCATABLE :: Fun(:)
    ALLOCATE(Child(4,20) :: Fun(10))
  END FUNCTION



