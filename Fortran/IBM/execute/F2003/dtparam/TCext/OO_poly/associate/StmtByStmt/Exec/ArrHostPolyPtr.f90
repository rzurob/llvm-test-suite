! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrHostPolyPtr.f
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 14, 2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  REFERENCE                  : Feature 219934
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is a host associate name
!*
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
      private
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
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

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4,*)) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base(4,*))  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child(4,*))
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM ArrHostPolyPtr
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20)), TARGET :: Tar

  CLASS(Child(4,:)), POINTER :: Arr(:)

  ALLOCATE(Arr(3), SOURCE=Child(4,20)(ChildId=-2, BaseId=-1))

  CALL Sub(Arr)

  CONTAINS

  FUNCTION Fun(Arr1, Arr2)
  CLASS(Child(4,:)), POINTER :: Fun(:)
  CLASS(Child(4,*)) :: Arr1(:), Arr2(:)
    ALLOCATE(Fun(SIZE(Arr)), SOURCE = Arr1)
    SELECT TYPE (Arr2)
    TYPE IS (Child(4,*))
      Arr2 =Arr1
    END SELECT
  END FUNCTION

  SUBROUTINE Sub(Arr)
  CLASS(Child(4,:)), POINTER :: Arr(:)
  TYPE (Child(4,20)) :: V(SIZE(Arr))

  ASSOCIATE ( As => V )
  ASSOCIATE ( As => Fun(Arr, As))
    SELECT TYPE ( As )
    CLASS IS (Child(4,*))

          IF (ANY(SHAPE(As) .NE. (/3/)))      STOP 33
          IF ( ANY(As%Base%GetId() .NE. -1) ) STOP 34
          IF ( ANY(As%GetId()      .NE. -2) ) STOP 35
          IF ( ANY(As%BaseId       .NE. -1) ) STOP 36
          IF ( ANY(As%ChildId      .NE. -2) ) STOP 37

          CALL As(1)%SetId(As)
          CALL As(1)%Base%SetId(As%Base)

          IF ( ANY(As%Base%GetId() .NE. 1 ) ) STOP 44
          IF ( ANY(As%GetId()      .NE. 2 ) ) STOP 45
          IF ( ANY(As%BaseId       .NE. 1 ) ) STOP 46
          IF ( ANY(As%ChildId      .NE. 2 ) ) STOP 47


    CLASS DEFAULT
      STOP 38

  END SELECT

  END ASSOCIATE

  IF (ANY(SHAPE(As) .NE. (/3/)))      STOP 53
  IF ( ANY(As%Base%GetId() .NE. -1) ) STOP 54
  IF ( ANY(As%GetId()      .NE. -2) ) STOP 55
  IF ( ANY(As%BaseId       .NE. -1) ) STOP 56
  IF ( ANY(As%ChildId      .NE. -2) ) STOP 57

  END ASSOCIATE

  END SUBROUTINE

  END



