! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/associate/CrossFeatures/DerTypeAbs.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  DerTypeAbs.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : DerTypeAbs
!*
!*  DATE                       : Mar. 07, 2005
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
!*    The selector is of a derived type extending from an abstract type
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT :: Base(N1,K1)    ! (20,4)
      INTEGER, KIND :: K1
      INTEGER, LEN  :: N1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (20,4)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(*,4)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(*,4)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE


  PROGRAM DerTypeAbs
  USE M, Abs => Base, DT => Child
  IMPLICIT NONE

  TYPE, EXTENDS(DT) :: T    ! (20,4)
  END TYPE

  CLASS(Abs(:,4)), POINTER :: V
  ALLOCATE(V, SOURCE=T(20,4)(ChildID=-2, BaseID=-1))

  ASSOCIATE( V => V )
  SELECT TYPE(V)
  CLASS IS (Abs(*,4))
  ASSOCIATE( V => V )

    SELECT TYPE (V)
    TYPE IS (T(*,4))

      IF ( V%BaseId       .NE. -1 ) STOP 31
      IF ( V%Base%BaseId  .NE. -1 ) STOP 32
      IF ( V%ChildId      .NE. -2 ) STOP 33
!     IF ( V%Base%GetId() .NE. -1 ) STOP 34  !C611
      IF ( V%DT%GetId() .NE. -2 ) STOP 35

    CLASS DEFAULT
      STOP 45
    END SELECT

  END ASSOCIATE

  CLASS DEFAULT
    STOP 40
  END SELECT
  END ASSOCIATE

  END


