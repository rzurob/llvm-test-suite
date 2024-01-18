! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltImplicit.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltImplicit.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltImplicit
!*
!*  DATE                       : Dec. 16, 2004
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
!*   The selector is a poly var of implicit type
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero) :: Base    ! (4,20)
      INTEGER(K1) :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4,20)
      INTEGER(K1)  :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

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

    SUBROUTINE Conv(CArg1, Carg2)
    IMPLICIT TYPE(Child(4,20))(C)
       CArg2 = CArg1
    END SUBROUTINE

  END MODULE

  PROGRAM SltImpilcit
  USE M
  IMPLICIT TYPE(Child(4,20))(C)

  CLASS(Child(4,:)), POINTER :: Ptr

  ALLOCATE(Ptr, SOURCE=C)
  SELECT TYPE (Ptr)
    TYPE IS (Child(4,*))
      Ptr%BaseId = -1
      Ptr%ChildId = -2
  END SELECT

  CALL Conv(C, Ptr)

  SELECT TYPE ( As => Ptr )
    CLASS DEFAULT
      STOP 20
    CLASS is (Child(4,*))
      STOP 24
    TYPE IS (Child(4,*))
      IF ( As%BaseId       .NE. 1 ) STOP 31
      IF ( As%ChildId      .NE. 2 ) STOP 32
      IF ( As%Base%GetId() .NE. 1 ) STOP 33
      IF ( As%GetId()      .NE. 2 ) STOP 34

  END SELECT

  END
