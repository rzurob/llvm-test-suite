! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/CrossFeatures/DerTypePriv.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  DerTypePriv.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : DerTypePriv
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
!*    The selector is of a derived type with private components
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE, ABSTRACT, PRIVATE ::  Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero) :: Base    ! (4,20)
      INTEGER(K1), PRIVATE :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1), PRIVATE  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    SUBROUTINE SetId(Arg1, Arg2)
    CLASS(Base(4,*))         :: Arg1
    INTEGER, INTENT(IN) :: Arg2
      SELECT TYPE(Arg1)
      TYPE IS (Base(4,*))
        Arg1%BaseId = Arg2
      TYPE IS (Child(4,*))
        Arg1%ChildId = Arg2
      END SELECT
    END SUBROUTINE

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM DerTypePriv
  USE M, DT2=>Child, DT1=>Base
  IMPLICIT NONE

  TYPE(DT2(4,20)), TARGET :: T=DT2(4,20)()

  ASSOCIATE( As => T )
  ASSOCIATE( V => T )

    IF ( As%Base%GetID() .NE. 1 ) STOP 20
    IF ( As%GetID()      .NE. 2 ) STOP 21

    CALL As%Base%SetID(-1)
    CALL As%SetID(-2)

    IF ( V%Base%GetID() .NE. -1 ) STOP 30
    IF ( V%GetID()      .NE. -2 ) STOP 31
  END ASSOCIATE
  END ASSOCIATE

  IF ( T%Base%GetID() .NE. -1 ) STOP 40
  IF ( T%GetID()      .NE. -2 ) STOP 41

  END


