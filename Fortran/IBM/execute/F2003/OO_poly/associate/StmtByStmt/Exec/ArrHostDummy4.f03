! *********************************************************************
!*  ===================================================================
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
!*    The selector is a host associate name
!*    Selector is a poly zero sized dummy array
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  MODULE M

    TYPE  :: Zero
      private
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base
      INTEGER :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, NoPASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child
      INTEGER  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, NoPASS   :: SetId => SetChildId
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
    CLASS(Base) :: Arg(:)
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Base)  :: Arg(:)
      SELECT TYPE(Arg)
        TYPE IS (Child)
          Arg%ChildId = -Arg%ChildId
      END SELECT
    END SUBROUTINE

  END MODULE


  PROGRAM ArrHostDummy4
  USE M
  IMPLICIT NONE
  INTEGER :: i

  TYPE, EXTENDS(Child) :: DT
  END TYPE

  TYPE (DT) :: V(6)

  V(1::2)%Child = (/(Child(ChildID=-2, BaseID=-1), i=1, 3)/)
  V(2::2)%Child = (/(Child(ChildID= 0, BaseID= 0), i=1, 3)/)

  CALL SWAP(V(1::2)%Child, V(2::2)%Child , SIZE(V(1::2)))

  IF ( ANY(V(1::2)%Child%BaseID  .NE. -1) ) ERROR STOP 10
  IF ( ANY(V(1::2)%Child%ChildID .NE. -2) ) ERROR STOP 11
  IF ( ANY(V(2::2)%Child%BaseID  .NE.  0) ) ERROR STOP 12
  IF ( ANY(V(2::2)%Child%ChildID .NE.  0) ) ERROR STOP 13

  CONTAINS

  SUBROUTINE SWAP(Arr1, Arr2, N )
  CLASS(*), INTENT(INOUT) :: Arr1(-1:-2), Arr2(0:-1)
  TYPE(Child)  :: Temp(0)
  INTEGER :: N

  ASSOCIATE ( As1 => Arr1, As2 => Arr2 )

    IF (ANY(SHAPE(As1)  .NE. (/0/)))            ERROR STOP 21
    IF (ANY(SHAPE(As2)  .NE. (/0/)))            ERROR STOP 22
    IF (ANY(LBOUND(As1) .NE. (/1/)))            ERROR STOP 23
    IF (ANY(LBOUND(As2) .NE. (/1/)))            ERROR STOP 24
    IF (ANY(UBOUND(As1) .NE. (/0/)))            ERROR STOP 25
    IF (ANY(UBOUND(As2) .NE. (/0/)))            ERROR STOP 26

!  Nothing will be changed below!

    SELECT TYPE(As1)
    TYPE IS (Child)
    SELECT TYPE(As2)
    TYPE IS (Child)

    ASSOCIATE ( As => As1(:) )

      IF (ANY(SHAPE(As) .NE. (/0/)))      ERROR STOP 33
      IF ( ANY(As%Base%GetId() .NE. -9) ) ERROR STOP 34
      IF ( ANY(As%GetId()      .NE. -8) ) ERROR STOP 35
      IF ( ANY(As%BaseId       .NE. -7) ) ERROR STOP 36
      IF ( ANY(As%ChildId      .NE. -6) ) ERROR STOP 37

      CALL As(1)%SetId(As)
      CALL As(1)%Base%SetId(As%Base)

      IF ( ANY(As%Base%GetId() .NE. 1 ) ) ERROR STOP 44
      IF ( ANY(As%GetId()      .NE. 2 ) ) ERROR STOP 45
      IF ( ANY(As%BaseId       .NE. 3 ) ) ERROR STOP 46
      IF ( ANY(As%ChildId      .NE. 4 ) ) ERROR STOP 47

    END ASSOCIATE

    ASSOCIATE ( As => As2(:) )

      IF (ANY(SHAPE(As) .NE. (/0/)))     ERROR STOP 53
      IF ( ANY(As%Base%GetId() .NE. 9) ) ERROR STOP 54
      IF ( ANY(As%GetId()      .NE. 8) ) ERROR STOP 55
      IF ( ANY(As%BaseId       .NE. 7) ) ERROR STOP 56
      IF ( ANY(As%ChildId      .NE. 6) ) ERROR STOP 57

      CALL As(1)%SetId(As)
      CALL As(1)%Base%SetId(As%Base)

      IF ( ANY(As%Base%GetId() .NE. 6 ) ) ERROR STOP 64
      IF ( ANY(As%GetId()      .NE. 7 ) ) ERROR STOP 65
      IF ( ANY(As%BaseId       .NE. 8 ) ) ERROR STOP 66
      IF ( ANY(As%ChildId      .NE. 9 ) ) ERROR STOP 67

    END ASSOCIATE

    Temp = As1
    As1 = As2
    As2 = temp

    END SELECT
    END SELECT
  END ASSOCIATE

  END SUBROUTINE

  END


