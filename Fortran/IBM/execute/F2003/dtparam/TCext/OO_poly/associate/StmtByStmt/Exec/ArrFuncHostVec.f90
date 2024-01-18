! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/ArrFuncHostVec.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ArrFuncHostVec.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ArrFuncHostVec
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
!*    with an associate name  associting to an array section
!*
!*    (Comp Failed)
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

    TYPE(Child(4,20)) FUNCTION Fun(Arg)
      TYPE(Child(4,*)) :: Arg(:)
      DIMENSION   :: Fun(SIZE(Arg):2*SIZE(Arg)-1)
        Fun = Arg
    END FUNCTION

  END MODULE


  PROGRAM ArrFuncHostVec
  USE M
  IMPLICIT NONE
  INTEGER :: i


  TYPE (Child(4,20)) :: V(6)
  INTEGER :: S(3)=(/3,2,1/)

  V(::2) = Child(4,20)(ChildID=-2, BaseID=-1)

  ASSOCIATE ( As1 => V(::2) )
  ASSOCIATE ( As2 => Fun(As1(S)) )

    IF (ANY(SHAPE(As1)  .NE. (/3/)))            STOP 21
    IF (ANY(LBOUND(As1) .NE. (/1/)))            STOP 23
    IF (ANY(UBOUND(As1) .NE. (/3/)))            STOP 25


    ASSOCIATE ( As => As2(:) )

      IF (ANY(SHAPE(As) .NE. (/3/)))      STOP 33
      IF ( ANY(As%Base%GetId() .NE. -1) ) STOP 34
      IF ( ANY(As%GetId()      .NE. -2) ) STOP 35
      IF ( ANY(As%BaseId       .NE. -1) ) STOP 36
      IF ( ANY(As%ChildId      .NE. -2) ) STOP 37

    END ASSOCIATE


  END ASSOCIATE
  END ASSOCIATE

  IF ( ANY(V(1::2)%BaseID  .NE. -1) ) STOP 10
  IF ( ANY(V(1::2)%ChildID .NE. -2) ) STOP 11
  IF ( ANY(V(2::2)%BaseID  .NE.  1) ) STOP 12
  IF ( ANY(V(2::2)%ChildID .NE.  2) ) STOP 13


  END



