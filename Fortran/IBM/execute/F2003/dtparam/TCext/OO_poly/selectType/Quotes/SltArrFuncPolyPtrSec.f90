! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_poly/selectType/Quotes/SltArrFuncPolyPtrSec.f
! opt variations: -qnok -qnol -qreuse=base

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 19, 2005
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
!*   The selector is an array pointer from function call
!*   forming a array section
!*    (Polymorphism-298420)
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    CONTAINS
      PROCEDURE, NoPASS   :: Called
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base(N2,K2)    ! (4,20,20,4)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      INTEGER(K2)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child(N3,K3)    ! (4,20,20,4,20,4)
      INTEGER, KIND :: K3
      INTEGER, LEN  :: N3
      INTEGER(K3)   :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    FUNCTION Called()
    LOGICAL :: Called
      Called =.true.
    END FUNCTION

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*,*,4,*,4)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*,*,4)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4,*,*,4)), INTENT(INOUT) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4,*,*,4)), INTENT(INOUT)  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4,*,*,4,*,4))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM SltArrFuncPolyPtrSec
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20,20,4,20,4)) :: V(4,4)

  V%BaseId = -1
  V%ChildId = -2

  CALL Sub(V(1:3,::2))

  CONTAINS

  FUNCTION Fun(Arg)
  CLASS(*) :: Arg(:,:)
  CLASS(*), POINTER :: Fun(:,:)
    ALLOCATE(Fun(SIZE(Arg,1),SIZE(Arg,2)), SOURCE=Arg)
  END FUNCTION

  SUBROUTINE Sub(Arg)
  CLASS(Base(4,*,*,4)) :: Arg(:, :)

    SELECT TYPE (V => RESHAPE(Fun(Arg(1::2,::1)),(/2,2/)))
    CLASS DEFAULT
      SELECT TYPE (V => RESHAPE(V,(/2,2/)))
        TYPE IS (Child(4,*,*,4,*,4))

          IF ( SIZE(V)          .NE. 4 )          STOP 21
          IF ( ANY (LBOUND(V)   .NE. (/1, 1/) ) ) STOP 30
          IF ( ANY (UBOUND(V)   .NE. (/2, 2/) ) ) STOP 31
          IF ( ANY(SHAPE(V)     .NE. (/2,2/)) )   STOP 20

          IF ( ANY(V%Base%GetId() .NE. -1) ) STOP 34
          IF ( ANY(V%GetId()      .NE. -2) ) STOP 35
          IF ( ANY(V%BaseId       .NE. -1) ) STOP 36
          IF ( ANY(V%ChildId      .NE. -2) ) STOP 37

          IF ( .NOT. V%Called() ) STOP 45

       CLASS DEFAULT
          STOP 40
        CLASS is (Child(4,*,*,4,*,4))
          STOP 56
        TYPE is (Base(4,*,*,4))
          STOP 57
        TYPE IS (Zero(4,*))
          STOP 58
      END SELECT

  END SELECT

  END SUBROUTINE


  END



