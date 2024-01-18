! GB DTP extension using:
! ftcx_dtp -qnol -qnodefaultpv -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltPriComp.f
! opt variations: -ql -qdefaultpv -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Dec. 14, 2004
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
!*   The type spec is specified with a type with private components
!*    (297038)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND        :: K1
      INTEGER(K1), PRIVATE :: BaseId=-1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, ABSTRACT,  EXTENDS(Base) :: Base1    ! (4)
      INTEGER(K1) :: Base1Id=1
    END TYPE

    TYPE, EXTENDS(Base1) :: Child    ! (4)
      PRIVATE
      TYPE(Base(K1))  :: Base1Comp=Base(K1)(BaseId=0)
    END TYPE

    CONTAINS

    FUNCTION GetBaseId(Arg)
    CLASS(Base(4))  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM SltPriComp
  USE M
  IMPLICIT NONE

  CLASS(*) ,ALLOCATABLE :: Var

  ALLOCATE(Var, SOURCE=Child(4)())

  SELECT TYPE (Var)
   CLASS IS (Base(4))
     STOP 20
   TYPE IS (Base(4))
     STOP 21
   CLASS IS (Child(4))
     STOP 22
   TYPE IS (Child(4))
     IF (Var%GetId() .NE. -1 )      STOP 31
     IF (Var%Base1%Base1Id .NE. 1 ) STOP 32
     IF (Var%Base1Id .NE. 1 )       STOP 33
  END SELECT

  END
