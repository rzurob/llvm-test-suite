! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=none /tstdev/OO_poly/selectType/Quotes/TypeMatch1.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=base

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 21, 2005
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
!*   Types specified for TYPE IS and CLASS IS are the same
!*
!*    ()
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
    CLASS(Base(4,*,*,4)), INTENT(INOUT) :: Arg(:,:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4,*,*,4)), INTENT(INOUT)  :: Arg(:,:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4,*,*,4,*,4))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM TypeMatch1
  USE M
  IMPLICIT NONE
  CLASS(Base(4,:,:,4)), POINTER :: V(:,:)

    ALLOCATE(V(2:3,3:4), SOURCE=Child(4,20,20,4,20,4)(BaseId=-1, ChildId=-2))

    ASSOCIATE  (W=>V)
    SELECT TYPE (U=>W)
    CLASS IS (Child(4,*,*,4,*,4))
       STOP 42
    CLASS IS (Base(4,*,*,4))
       STOP 43
    TYPE IS (Child(4,*,*,4,*,4))
      ! Check U
      IF ( SIZE(U)          .NE. 4 )          ERROR STOP 31
      IF ( ANY (LBOUND(U)   .NE. (/2, 3/) ) ) ERROR STOP 32
      IF ( ANY (UBOUND(U)   .NE. (/3, 4/) ) ) ERROR STOP 33
      IF ( ANY(SHAPE(U)     .NE. (/2,2/)) )   ERROR STOP 24
      IF ( ANY(U%Base%GetId() .NE. -1) )      ERROR STOP 35
      IF ( ANY(U%GetId()      .NE. -2) )      ERROR STOP 36
      IF ( ANY(U%BaseId       .NE. -1) )      ERROR STOP 37
      IF ( ANY(U%ChildId      .NE. -2) )      ERROR STOP 38

      IF ( .NOT. U%Called() ) ERROR STOP 45

    TYPE IS (Base(4,*,*,4))
       STOP 40
    CLASS DEFAULT
       STOP 41
    END SELECT
    END ASSOCIATE

  END



