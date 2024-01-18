! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltHostVarImplicit1.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltHostVarImplicit1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltHostVarImplicit1
!*
!*  DATE                       : Dec. 23, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Select Type
!*
!*  SECONDARY FUNCTIONS TESTED : Selector
!*
!*  REFERENCE                  : Feature 219934.OO_poly
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*
!*   The selector is an associate name associating to a dummy of implicit type
!*    (Ice )
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
      CLASS(Base(K1,:)), POINTER :: BasePtr => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: SetId => SetChildId
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
    CLASS(Base(4,*))  :: Arg
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4,*))  :: Arg
      Arg%ChildId = -Arg%ChildId
    END SUBROUTINE

  END MODULE


  PROGRAM SltHostVarImplicit1
  USE M

  CALL Sub(Child(4,20)())

  CONTAINS

  SUBROUTINE Sub(V)
  IMPLICIT CLASS(*)(V), TYPE(Zero(4,20))(A)

  SELECT TYPE ( As => V )
    CLASS IS (Child(4,*))
      IF ( As%Base%GetId() .NE.  1 ) STOP 34
      IF ( As%GetId()      .NE.  2 ) STOP 35
      IF ( As%BaseId       .NE.  1 ) STOP 36
      IF ( As%ChildId      .NE.  2 ) STOP 37
      CALL As%SetId()
      CALL As%Base%SetId()
      IF ( As%Base%GetId() .NE. -1 ) STOP 34
      IF ( As%GetId()      .NE. -2 ) STOP 35
      IF ( As%BaseId       .NE. -1 ) STOP 36
      IF ( As%ChildId      .NE. -2 ) STOP 37

    TYPE is (Base(4,*))
      STOP 32
    TYPE IS (Zero(4,*))
      STOP 38

  END SELECT

  END SUBROUTINE

  END

