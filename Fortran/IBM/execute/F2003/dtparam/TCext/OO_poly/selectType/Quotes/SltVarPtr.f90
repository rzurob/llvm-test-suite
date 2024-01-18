! GB DTP extension using:
! ftcx_dtp -qk -ql -qreuse=none /tstdev/OO_poly/selectType/Quotes/SltVarPtr.f
! opt variations: -qnok -qnol -qreuse=base

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltVarPtr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltTypeVarPtr
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
!*   The selector is a poly pointer var.
!*    (Wrong result:297129 )
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero) :: Base(N2,K2)    ! (4,20,20,4)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      INTEGER(K2)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child(N3,K3)    ! (4,20,20,4,20,4)
      INTEGER, KIND :: K3
      INTEGER, LEN  :: N3
      INTEGER(K3)   :: ChildId = 2
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*,*,4,*,4)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*,*,4)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM SltVarPtr
  USE M
  IMPLICIT NONE

  CLASS(*), POINTER :: Ptr

  ALLOCATE(Child(4,20,20,4,20,4) :: Ptr)
  Ptr => Ptr

  SELECT TYPE ( Ptr )
    CLASS DEFAULT

      ASSOCIATE ( As => Ptr  )
      SELECT TYPE (As)
        CLASS DEFAULT
          STOP 20
        TYPE IS (CHARACTER(*))
          STOP 21
        TYPE IS (Base(4,*,*,4))
          STOP 22
        CLASS IS (Base(4,*,*,4))
          STOP 23
        CLASS is (Child(4,*,*,4,*,4))
          STOP 24
        TYPE IS (Child(4,*,*,4,*,4))
          IF ( As%Base%GetId() .NE. 1 ) STOP 34
          IF ( As%GetId()      .NE. 2 ) STOP 35
      END SELECT
      END ASSOCIATE

  END SELECT

  END
