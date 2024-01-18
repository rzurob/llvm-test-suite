! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/HostAssocVarPtr.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  HostAssocVarPtr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : HostAssocVarPtr
!*
!*  DATE                       : Nov. 02, 2004
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
!*    The selector is a non poly pointer variable of derived types
!*    (ICE)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: BaseId = 1
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
      TYPE(Base(K1)), POINTER  :: BasePtr
      TYPE(Child(K1)), POINTER :: ChildPtr
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    ELEMENTAL FUNCTION GetChildId(Arg)
    CLASS(Child(4)), INTENT(IN) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    ELEMENTAL FUNCTION GetBaseId(Arg)
    CLASS(Base(4)), INTENT(IN) :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM HostAssocVarPtr
  USE M
  IMPLICIT NONE

  TYPE(Base(4)),  TARGET      :: V
  TYPE(Child(4)), TARGET      :: U
  TYPE(Child(4)), TARGET      :: W(3)
  TYPE(Child(4)), POINTER     :: Ptr
  TYPE(Child(4)), POINTER     :: PtrArr(:)

  Ptr => U
  PtrArr => W

  ASSOCIATE ( T  => Ptr )
  ASSOCIATE ( As => T  )
    IF ( As%GetID() .NE. 2) STOP 50
    IF ( As%ChildId .NE. 2) STOP 51
    IF ( As%BaseId .NE. 1) STOP 52
    ASSOCIATE ( As2 => As%Base )
      IF ( As2%GetID() .NE. 1 ) STOP 53
    END ASSOCIATE

    U%BaseId = -1
    IF ( As%BaseId .NE. -1) STOP 54

    As%BasePtr => V
    IF ( As%BasePtr%BaseId .NE. 1) STOP 55

    As%ChildPtr => Ptr  ! itself
    IF ( As%ChildPtr%BaseId .NE. -1) STOP 56
    IF ( As%ChildPtr%ChildId .NE. 2) STOP 57

  END ASSOCIATE
  END ASSOCIATE


  ASSOCIATE ( T  => PtrArr )
  ASSOCIATE ( As => T  )
    IF ( ANY(As%GetID() .NE. 2)) STOP 60
    IF ( ANY(As%ChildId .NE. 2)) STOP 61
    IF ( ANY(As%BaseId  .NE. 1)) STOP 62
    ASSOCIATE ( As2 => As%Base )
      IF ( ANY(As2%GetID() .NE. 1) ) STOP 63
    END ASSOCIATE

    T%BaseId = -1
    IF ( ANY(As%BaseId .NE. -1)) STOP 64

  END ASSOCIATE
  END ASSOCIATE


  END
