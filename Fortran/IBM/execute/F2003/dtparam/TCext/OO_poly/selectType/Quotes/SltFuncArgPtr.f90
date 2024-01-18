! GB DTP extension using:
! ftcx_dtp -qk -ql -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltFuncArgPtr.f
! opt variations: -qnok -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltFuncArgPtr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltFuncArgPtr
!*
!*  DATE                       : Jan. 06, 2005
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
!*   The selector is a function return with associate name(to a pointer) as argument
!*    (297727)
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


  PROGRAM SltFuncArgPtr
  USE M
  IMPLICIT NONE
  TYPE(Child(4,20)), TARGET :: Tar

  SELECT TYPE( As => Fun1(Tar))
  END SELECT

  CONTAINS

  FUNCTION Fun0(Arg)
  CLASS(Zero(4,*)), TARGET ::Arg
  CLASS(*), POINTER :: Fun0
    Fun0 => Arg
  END FUNCTION

  RECURSIVE FUNCTION Fun1(Arg)
  CLASS(Zero(4,*)), TARGET ::Arg
  CLASS(Zero(4,:)), POINTER :: Fun1

  SELECT TYPE ( As0 => Fun0(Arg) )
    CLASS IS (Child(4,*))
      SELECT TYPE (As => Fun0(As0))
        TYPE IS (Child(4,*))
          IF ( As%Base%GetId() .NE. 1 ) STOP 34
          IF ( As%GetId()      .NE. 2 ) STOP 35
          IF ( As%BaseId       .NE. 1 ) STOP 36
          IF ( As%ChildId      .NE. 2 ) STOP 37

          CALL As0%SetId()
          CALL As0%Base%SetId()

          IF ( As%Base%GetId() .NE. -1 ) STOP 44
          IF ( As%GetId()      .NE. -2 ) STOP 45
          IF ( As%BaseId       .NE. -1 ) STOP 46
          IF ( As%ChildId      .NE. -2 ) STOP 47
       CLASS DEFAULT
          STOP 40
      END SELECT

    TYPE is (Base(4,*))
      STOP 32
    TYPE IS (Zero(4,*))
      STOP 38

  END SELECT

  Fun1 => NULL()

  END FUNCTION

  END

