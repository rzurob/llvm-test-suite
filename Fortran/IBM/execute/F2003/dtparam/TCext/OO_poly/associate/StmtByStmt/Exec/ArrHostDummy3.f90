! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrHostDummy3.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ArrHostDummy3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ArrHostDummy3
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
!*    Selector is a ploy assumed size  dummy array
!*    (299886)
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


  PROGRAM ArrHostDummy3
  USE M
  IMPLICIT NONE
  INTEGER :: i

  TYPE, EXTENDS(Child) :: DT    ! (4,20)
  END TYPE

  TYPE (DT(4,20)) :: V(6)

  V(1::2)%Child = (/(Child(4,20)(ChildID=-2, BaseID=-1), i=1, 3)/)
  V(2::2)%Child = (/(Child(4,20)(ChildID= 0, BaseID= 0), i=1, 3)/)

  CALL SWAP(V(1::2)%Child, V(2::2)%Child , SIZE(V(1::2)))

  IF ( ANY(V(1::2)%Child%BaseID  .NE. 0) ) STOP 20
  IF ( ANY(V(1::2)%Child%ChildID .NE. 0) ) STOP 21
  IF ( ANY(V(2::2)%Child%BaseID  .NE. 1) ) STOP 22
  IF ( ANY(V(2::2)%Child%ChildID .NE. 2) ) STOP 23

  CONTAINS

  SUBROUTINE SWAP(Arr1, Arr2, N )
  CLASS(*), INTENT(INOUT) :: Arr1(*), Arr2(*)
  TYPE(Child(4,20))  :: Temp(3)
  INTEGER :: N


  ASSOCIATE ( As1 => Arr1(1:N), As2 => Arr2(1:N) )
    IF (ANY(SHAPE(As1)  .NE. (/3/)))            STOP 31
    IF (ANY(SHAPE(As2)  .NE. (/3/)))            STOP 32
    IF (ANY(UBOUND(As1) .NE. (/3/)))            STOP 31
    IF (ANY(UBOUND(As2) .NE. (/3/)))            STOP 32

    SELECT TYPE(As1)
    TYPE IS (Child(4,*))
    SELECT TYPE(As2)
    TYPE IS (Child(4,*))

    ASSOCIATE ( As => As1(:) )

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

    END ASSOCIATE

    ASSOCIATE ( As => As2(:) )

      IF (ANY(SHAPE(As) .NE. (/3/)))     STOP 53
      IF ( ANY(As%Base%GetId() .NE. 0) ) STOP 54
      IF ( ANY(As%GetId()      .NE. 0) ) STOP 55
      IF ( ANY(As%BaseId       .NE. 0) ) STOP 56
      IF ( ANY(As%ChildId      .NE. 0) ) STOP 57

      CALL As(1)%SetId(As)
      CALL As(1)%Base%SetId(As%Base)

      IF ( ANY(As%Base%GetId() .NE. 0 ) ) STOP 64
      IF ( ANY(As%GetId()      .NE. 0 ) ) STOP 65
      IF ( ANY(As%BaseId       .NE. 0 ) ) STOP 66
      IF ( ANY(As%ChildId      .NE. 0 ) ) STOP 67

    END ASSOCIATE

    Temp = As1
    As1 = As2
    As2 = temp

    END SELECT
    END SELECT
  END ASSOCIATE

  END SUBROUTINE

  END



