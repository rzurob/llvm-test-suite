! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrHostAlloc.f
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


  PROGRAM ArrHostAlloc
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20)), TARGET :: Tar

  CLASS(Child(4,:)), ALLOCATABLE :: Arr(:)

  ALLOCATE(Arr(3), SOURCE=Child(4,20)(ChildId=-2, BaseId=-1))

  ASSOCIATE ( As => Arr(:))
    SELECT TYPE ( As => As)
    CLASS IS (Child(4,*))
      ASSOCIATE (As => As(:), As1 => Arr)

          IF ( ANY(As%Base%GetId() .NE. -1) ) ERROR STOP 34
          IF ( ANY(As%GetId()      .NE. -2) ) ERROR STOP 35
          IF ( ANY(As%BaseId       .NE. -1) ) ERROR STOP 36
          IF ( ANY(As%ChildId      .NE. -2) ) ERROR STOP 37

          CALL As1(1)%SetId(As1)
          CALL As1(1)%Base%SetId(As1%Base)

          IF ( ANY(As%Base%GetId() .NE. 1 ) ) ERROR STOP 44
          IF ( ANY(As%GetId()      .NE. 2 ) ) ERROR STOP 45
          IF ( ANY(As%BaseId       .NE. 1 ) ) ERROR STOP 46
          IF ( ANY(As%ChildId      .NE. 2 ) ) ERROR STOP 47

      END ASSOCIATE

    CLASS DEFAULT
      STOP 38

  END SELECT

  END ASSOCIATE

  END



