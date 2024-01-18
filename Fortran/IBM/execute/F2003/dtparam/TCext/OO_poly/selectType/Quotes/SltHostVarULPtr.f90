! GB DTP extension using:
! ftcx_dtp -qk -ql -qnodefaultpv -qdeferredlp -qreuse=none /tstdev/OO_poly/selectType/Quotes/SltHostVarULPtr.f
! opt variations: -qnok -qnol -qdefaultpv -qnodeferredlp -qreuse=self -qreuse=base

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Jan. 05, 2005
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
!*   The selector is an associate name associating to unlimited poly pointer
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M

    TYPE  :: Zero(K1,N1)    ! (4,20)
        INTEGER, KIND :: K1
        INTEGER, LEN  :: N1
    END TYPE

    TYPE, EXTENDS(Zero)  :: Base(N2,K2)    ! (4,20,20,4)
      INTEGER, KIND :: K2
      INTEGER, LEN  :: N2
      INTEGER(K2)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
      PROCEDURE, PASS   :: SetId => SetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child(N3,K3)    ! (4,20,20,4,20,4)
      INTEGER, KIND                   :: K3
      INTEGER, LEN                    :: N3
      INTEGER(K3)                     :: ChildId = 2
      CLASS(Base(K3,:,:,K3)), POINTER :: BasePtr => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
      PROCEDURE, PASS   :: SetId => SetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4,*,*,4,*,4)), INTENT(IN) :: Arg
    INTEGER                  :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4,*,*,4)), INTENT(IN)  :: Arg
    INTEGER                  :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

    SUBROUTINE SetBaseId(Arg)
    CLASS(Base(4,*,*,4))  :: Arg
      Arg%BaseId =  -Arg%BaseId
    END SUBROUTINE

    SUBROUTINE SetChildId(Arg)
    CLASS(Child(4,*,*,4,*,4))  :: Arg
      Arg%ChildId = -Arg%ChildId
    END SUBROUTINE

  END MODULE


  PROGRAM SltHostVarULPtr
  USE M
  IMPLICIT NONE
  CLASS(*), POINTER :: Ptr
  TYPE(Child(4,20,20,4,20,4)), TARGET :: Tar
  TYPE(Child(4,20,20,4,20,4)), TARGET :: Tar1=Child(4,20,20,4,20,4)(BaseId=-1, ChildId=-2)

  Ptr => Tar

  SELECT TYPE ( As => Ptr  )
    CLASS IS (Zero(4,*))
      SELECT TYPE (As)
        TYPE IS (Child(4,*,*,4,*,4))
          Tar = Tar1
          IF ( As%Base%GetId() .NE. -1 ) ERROR STOP 34
          IF ( As%GetId()      .NE. -2 ) ERROR STOP 35
          IF ( As%BaseId       .NE. -1 ) ERROR STOP 36
          IF ( As%ChildId      .NE. -2 ) ERROR STOP 37
          CALL As%SetId()
          CALL As%Base%SetId()
          IF ( As%Base%GetId() .NE. 1 ) ERROR STOP 34
          IF ( As%GetId()      .NE. 2 ) ERROR STOP 35
          IF ( As%BaseId       .NE. 1 ) ERROR STOP 36
          IF ( As%ChildId      .NE. 2 ) ERROR STOP 37
       CLASS DEFAULT
          STOP 40
      END SELECT

    TYPE is (Base(4,*,*,4))
      STOP 32
    TYPE IS (Zero(4,*))
      STOP 38

  END SELECT

  END

