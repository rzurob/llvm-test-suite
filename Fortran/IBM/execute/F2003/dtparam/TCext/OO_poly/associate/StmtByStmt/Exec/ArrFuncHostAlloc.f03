! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrFuncHostAlloc.f
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
!*    The selector is a function call
!*    with an associate name  associting to an allocatable array
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
      TYPE (Base(K1,:)), ALLOCATABLE :: BS
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

    TYPE(Child(4,20)) FUNCTION Fun(Arg)
      TYPE(Child(4,*)) :: Arg(:)
      DIMENSION   :: Fun(SIZE(Arg):2*SIZE(Arg)-1)
        Fun = Arg
    END FUNCTION

  END MODULE


  PROGRAM ArrFuncHostAlloc
  USE M
  IMPLICIT NONE
  INTEGER :: i


  TYPE (Child(4,:)), ALLOCATABLE :: V(:)
  INTEGER :: S(3)=(/3,2,1/)
  TYPE (Base(4,20)) :: Temp(3)

  ALLOCATE(Child(4,20) :: V(6))
  V(::2) = Child(4,20)(ChildID=-2, BaseID=-1, BS=NULL())

  ASSOCIATE ( As1 => V(1::2), As2=> V(2::2) )

    IF (ANY(SHAPE(As1)  .NE. (/3/)))            ERROR STOP 21
    IF (ANY(LBOUND(As1) .NE. (/1/)))            ERROR STOP 22
    IF (ANY(UBOUND(As1) .NE. (/3/)))            ERROR STOP 23

    IF (ANY(SHAPE(As2)  .NE. (/3/)))            ERROR STOP 24
    IF (ANY(LBOUND(As2) .NE. (/1/)))            ERROR STOP 25
    IF (ANY(UBOUND(As2) .NE. (/3/)))            ERROR STOP 26

    IF ( ANY(As1%Base%GetId() .NE. -1) ) ERROR STOP 34
    IF ( ANY(As1%GetId()      .NE. -2) ) ERROR STOP 35
    IF ( ANY(As1%BaseId       .NE. -1) ) ERROR STOP 36
    IF ( ANY(As1%ChildId      .NE. -2) ) ERROR STOP 37

    IF ( ANY(As2%Base%GetId() .NE.  1) ) ERROR STOP 44
    IF ( ANY(As2%GetId()      .NE.  2) ) ERROR STOP 45
    IF ( ANY(As2%BaseId       .NE.  1) ) ERROR STOP 46
    IF ( ANY(As2%ChildId      .NE.  2) ) ERROR STOP 47

    Temp = As1%Base
    As1%Base = As2%Base
    As2%Base = Temp

  END ASSOCIATE

  IF ( ANY(V(1::2)%BaseID  .NE.  1) ) ERROR STOP 10
  IF ( ANY(V(1::2)%ChildID .NE. -2) ) ERROR STOP 11
  IF ( ANY(V(2::2)%BaseID  .NE. -1) ) ERROR STOP 12
  IF ( ANY(V(2::2)%ChildID .NE.  2) ) ERROR STOP 13


  END


