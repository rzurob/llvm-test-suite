! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrFuncHostMixed.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ArrFuncHostMixed.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ArrFuncHostDummy
!*
!*  DATE                       : Feb 16, 2005
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
!*    The selector is a mixed expr
!*    with associate name involved.
!*
!*    (300125)
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

    INTERFACE OPERATOR ( + )

      FUNCTION MyPlus1 (Arg1, Arg2)
        IMPORT ZERO, Base
        TYPE(Zero(4,*)), INTENT(IN) :: Arg1(:)
        TYPE(Base(4,*)), INTENT(IN) :: Arg2(:)
        TYPE(Base(4,20)) :: MyPlus1(SIZE(Arg1))
      END FUNCTION

      FUNCTION MyPlus2 (Arg1, Arg2)
        IMPORT Base, Child
        CLASS(Base(4,*)),  INTENT(IN) :: Arg2(:)
        CLASS(Child(4,*)), INTENT(IN) :: Arg1(:)
        TYPE(Child(4,20)) :: MyPlus2(SIZE(Arg1))
      END FUNCTION

    END INTERFACE OPERATOR ( + )


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


  PROGRAM ArrFuncHostMixed
  USE M
  IMPLICIT NONE
  INTEGER :: i
  CLASS (Child(4,:)), POINTER :: V(:)


  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS (Child(4,:)), POINTER :: Arg(:)
  TYPE (Child(4,20)) :: Temp(3)

  ALLOCATE(child(4,20) :: Arg(6))

  SELECT TYPE (Arg)
  TYPE IS (Child(4,*))
    Arg(1::2) = Child(4,20)(ChildID=-2, BaseID=-1)
    !Arg(2::2) = Child(ChildID= 2, BaseID= 1)  ! unnecessary
  END SELECT

  ASSOCIATE ( As => Arg(1:) + ( Arg(1:)%Base%Zero + Arg(1:)%Base ))

    IF (ANY(SHAPE(As)  .NE. (/6/)))      STOP 21
    IF (ANY(LBOUND(As) .NE. (/1/)))      STOP 22
    IF (ANY(UBOUND(As) .NE. (/6/)))      STOP 23

    ASSOCIATE ( As1 => As(::2), &
           &    As2 => As(2::2)+ (As(2::2)%Base%Zero + As(2::2)%Base) )

      IF ( ANY(As1%Base%GetId() .NE. -1) ) STOP 34
      IF ( ANY(As1%GetId()      .NE. -2) ) STOP 35
      IF ( ANY(As1%BaseId       .NE. -1) ) STOP 36
      IF ( ANY(As1%ChildId      .NE. -2) ) STOP 37

      IF ( ANY(As2%Base%GetId() .NE.  1) ) STOP 44
      IF ( ANY(As2%GetId()      .NE.  2) ) STOP 45
      IF ( ANY(As2%BaseId       .NE.  1) ) STOP 46
      IF ( ANY(As2%ChildId      .NE.  2) ) STOP 47

    END ASSOCIATE

  END ASSOCIATE

  IF ( .NOT. ASSOCIATED(V) )          STOP 14
  IF ( SIZE(V) .NE. 6 )               STOP 15
  IF ( ANY(V(1::2)%BaseID  .NE. -1) ) STOP 10
  IF ( ANY(V(1::2)%ChildID .NE. -2) ) STOP 11
  IF ( ANY(V(2::2)%BaseID  .NE.  1) ) STOP 12
  IF ( ANY(V(2::2)%ChildID .NE.  2) ) STOP 13

  END SUBROUTINE

  END


  FUNCTION MyPlus1 (Arg1, Arg2)
  USE M, ONLY : Zero, Base
  TYPE(Zero(4,*)), INTENT(IN) :: Arg1(:)
  TYPE(Base(4,*)), INTENT(IN) :: Arg2(:)
  TYPE(Base(4,20)) :: MyPlus1(SIZE(Arg1))
    MyPlus1 = Arg2
  END FUNCTION


  FUNCTION MyPlus2 (Arg1, Arg2)
  USE M, ONLY : Base, Child
  CLASS(Base(4,*)), INTENT(IN)  :: Arg2(:)
  CLASS(Child(4,*)), INTENT(IN) :: Arg1(:)
  TYPE(Child(4,20)) :: MyPlus2(SIZE(Arg1))
    MyPlus2      = Arg1
    MyPlus2%Base = Arg2
  END FUNCTION



