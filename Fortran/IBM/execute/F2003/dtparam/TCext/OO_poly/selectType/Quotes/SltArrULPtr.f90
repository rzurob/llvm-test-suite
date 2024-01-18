! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltArrULPtr.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltArrULPtr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltArrPtr
!*
!*  DATE                       : Jan. 07, 2005
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
!*   The selector is an  unlimited polymorphic array pointer
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero(K1)    ! (4)
        INTEGER, KIND :: K1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base    ! (4)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4)) :: Arg(:,:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4))  :: Arg(:,:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrULPtr
  USE M
  IMPLICIT NONE
  TYPE(Child(4)), TARGET :: Arr(20,20,20)
  CLASS(*), POINTER :: Ptr(:,:,:)

  Ptr => Arr(2:19, 2:19:2, 2:19)

  SELECT TYPE ( Ptr )
    CLASS DEFAULT
      SELECT TYPE (Ptr=>Ptr(::2,:,:))
      CLASS IS (Zero(4))
      SELECT TYPE (Ptr)
        TYPE IS (Child(4))
          IF ( ANY (LBOUND(Ptr)     .NE. (/1, 1, 1/) ) )    STOP 30
          IF ( ANY (UBOUND(Ptr)     .NE. (/9, 9, 18/) ) ) STOP 31
          IF ( ANY (SHAPE(Ptr)      .NE. (/9, 9, 18/) ) ) STOP 32

          IF ( ANY(Ptr%Base%GetId() .NE. 1) ) STOP 34
          IF ( ANY(Ptr%GetId()      .NE. 2) ) STOP 35
          IF ( ANY(Ptr%BaseId       .NE. 1) ) STOP 36
          IF ( ANY(Ptr%ChildId      .NE. 2) ) STOP 37

          CALL Ptr(2,2,2)%SetId(Ptr)
          CALL Ptr(2,2,2)%Base%SetId(Ptr%Base)

          IF ( ANY(Ptr%Base%GetId() .NE. -1 ) ) STOP 44
          IF ( ANY(Ptr%GetId()      .NE. -2 ) ) STOP 45
          IF ( ANY(Ptr%BaseId       .NE. -1 ) ) STOP 46
          IF ( ANY(Ptr%ChildId      .NE. -2 ) ) STOP 47

       CLASS DEFAULT
          STOP 40
        CLASS is (Child(4))
          STOP 56
        TYPE is (Base(4))
          STOP 57
        TYPE IS (Zero(4))
          STOP 58
      END SELECT
      END SELECT

  END SELECT

  END

