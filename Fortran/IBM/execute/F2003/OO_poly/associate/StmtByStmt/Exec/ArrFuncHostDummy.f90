! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ArrFuncHostDummy.f
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
!*    The selector is a function call
!*    with an associate name  associting to a dummy array
!*
!*    (ICE)
!*    (301436-wrong result 34)
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
      TYPE (Child), POINTER :: BS
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

    TYPE(Child) FUNCTION Fun(Arg)
      TYPE(Child) :: Arg(:)
      DIMENSION   :: Fun(SIZE(Arg):2*SIZE(Arg)-1)
        Fun = Arg
    END FUNCTION

  END MODULE


  PROGRAM ArrFuncHostDummy
  USE M
  IMPLICIT NONE
  INTEGER :: i
  TYPE (Child), TARGET  :: V(6)

  V(::2) = Child(ChildID=-2, BaseID=-1, BS=NULL())

  CALL Sub(V)

  CONTAINS

  SUBROUTINE Sub(Arg)
  CLASS (Child), TARGET  :: Arg(:)
  TYPE (Child), POINTER :: Ptr(:)
  TYPE (Base) :: Temp(3)

  Ptr => Arg(2::2)

  ASSOCIATE ( As1 => Arg(1::2), As2=> Ptr )

    IF (ANY(SHAPE(As1)  .NE. (/3/)))            STOP 21
    IF (ANY(LBOUND(As1) .NE. (/1/)))            STOP 22
    IF (ANY(UBOUND(As1) .NE. (/3/)))            STOP 23

    IF (ANY(SHAPE(As2)  .NE. (/3/)))            STOP 24
    IF (ANY(LBOUND(As2) .NE. (/1/)))            STOP 25
    IF (ANY(UBOUND(As2) .NE. (/3/)))            STOP 26

    ASSOCIATE ( As1 => Fun(FUN(As2)), As2 =>Fun(Fun(As1)) )

      IF ( ANY(As1%Base%GetId() .NE.  1) ) STOP 34
      IF ( ANY(As1%GetId()      .NE.  2) ) STOP 35
      IF ( ANY(As1%BaseId       .NE.  1) ) STOP 36
      IF ( ANY(As1%ChildId      .NE.  2) ) STOP 37

      IF ( ANY(As2%Base%GetId() .NE. -1) ) STOP 44
      IF ( ANY(As2%GetId()      .NE. -2) ) STOP 45
      IF ( ANY(As2%BaseId       .NE. -1) ) STOP 46
      IF ( ANY(As2%ChildId      .NE. -2) ) STOP 47

    END ASSOCIATE

    Temp = As1%Base
    As1%Base = As2%Base
    As2%Base = Temp

  END ASSOCIATE

  IF ( ANY(V(1::2)%BaseID  .NE.  1) ) STOP 10
  IF ( ANY(V(1::2)%ChildID .NE. -2) ) STOP 11
  IF ( ANY(V(2::2)%BaseID  .NE. -1) ) STOP 12
  IF ( ANY(V(2::2)%ChildID .NE.  2) ) STOP 13

  END SUBROUTINE

  END



