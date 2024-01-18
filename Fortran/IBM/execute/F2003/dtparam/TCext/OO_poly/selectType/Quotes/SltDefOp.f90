! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=base /tstdev/OO_poly/selectType/Quotes/SltDefOp.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=none

! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: SltDefOp.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : SltDefOp
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
!*   The selector is a poly expr with defined operator
!*    (297388)
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

    INTERFACE OPERATOR ( .OP. )
      FUNCTION MyAdd (Arg1, Arg2)
        IMPORT Base, Child
        CLASS(Base(4,*)),  INTENT(IN) :: Arg1
        CLASS(*),     INTENT(IN) :: Arg2
        CLASS(*),     POINTER    :: MyAdd
      END FUNCTION
    END INTERFACE OPERATOR ( .OP. )

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
      Arg%BaseId = -1
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4,*))  :: Arg
      Arg%ChildId = -2
    END SUBROUTINE

  END MODULE


  PROGRAM SltDefOp
  USE M
  IMPLICIT  NONE

  Call Sub(Child(4,20)(), Child(4,20)())

  CONTAINS

  SUBROUTINE Sub(Arg1, Arg2)
  CLASS(Base(4,*)) :: Arg1
  CLASS(Zero(4,*)) :: Arg2

  SELECT TYPE ( As=> Arg1 .OP. Arg2 )
    CLASS DEFAULT
      STOP 30
    TYPE is (Base(4,*))
      STOP 32
    CLASS IS (Child(4,*))
      IF ( As%Base%GetId() .NE.  2 ) STOP 34
      IF ( As%GetId()      .NE.  4 ) STOP 35
      IF ( As%BaseId       .NE.  2 ) STOP 36
      IF ( As%ChildId      .NE.  4 ) STOP 37
    CLASS IS (Zero(4,*))
      STOP 38
  END SELECT
  END SUBROUTINE

  END

  FUNCTION MyAdd (Arg1, Arg2)
  USE M, ONLY: Base, Child
    CLASS(Base(4,*)),  INTENT(IN) :: Arg1
    CLASS(*),     INTENT(IN) :: Arg2
    CLASS(*),     POINTER    :: MyAdd

    ALLOCATE(Child(4,20)::MyAdd)

    SELECT TYPE (MyAdd)
      TYPE IS (Child(4,*))
        SELECT TYPE (Arg1)
          TYPE IS (Child(4,*))
            SELECT TYPE (Arg2)
              TYPE IS (Child(4,*))
                MyAdd%BaseId  = Arg1%BaseId  + Arg2%BaseId
                MyAdd%ChildId = Arg1%ChildId + Arg2%ChildId
            END SELECT
        END SELECT
    END SELECT
  END FUNCTION

