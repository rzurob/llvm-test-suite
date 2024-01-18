! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/VarPtr.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  VarPtr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : VarPtr
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
!*    ()
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
      TYPE(Base(K1)), POINTER :: BaseComp
    CONTAINS
      PROCEDURE, NOPASS :: PrintType => PrintChild
      PROCEDURE, PASS   :: GetId => GetChildId
    END TYPE

    CONTAINS

    SUBROUTINE PrintChild()
      PRINT *,'Child'
    END SUBROUTINE

    FUNCTION GetChildId(Arg)
    CLASS(Child(4)) :: Arg
    INTEGER      :: GetChildId
      GetChildId = Arg%ChildId
    END FUNCTION

    FUNCTION GetBaseId(Arg)
    CLASS(Base(4))  :: Arg
    INTEGER      :: GetBaseId
      GetBaseId = Arg%BaseId
    END FUNCTION

  END MODULE

  PROGRAM VarPtr
  USE M
  IMPLICIT NONE

  TYPE(Child(4)), TARGET      :: U
  TYPE(Child(4)), POINTER     :: Ptr
  TYPE(Child(4)), POINTER     :: Ptr0

  Ptr => U
  Ptr0 => Ptr
  ALLOCATE(Ptr0%BaseComp)

  ASSOCIATE ( As => Ptr )
    IF ( As%GetID() .NE. 2) STOP 50
    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( As0 .NE. 2) STOP 51
       IF ( As1 .NE. 1) STOP 52
    END ASSOCIATE
    ASSOCIATE ( As2 => As%Base )
      IF ( As2%GetID() .NE. 1 ) STOP 53
    END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE (As =>  Ptr%GetID())
    IF ( As .NE. 2 ) STOP 60
  END ASSOCIATE

  ASSOCIATE (As =>  Ptr%Base%GetID())
    IF ( As .NE. 1 ) STOP 61
  END ASSOCIATE

  ASSOCIATE (As =>  Ptr%BaseComp%GetID())
    IF ( As .NE. 1 ) STOP 61
  END ASSOCIATE

  ASSOCIATE ( As => Ptr0 )
    IF ( As%GetID() .NE. 2) STOP 50
    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( As0 .NE. 2) STOP 51
       IF ( As1 .NE. 1) STOP 52
    END ASSOCIATE
    ASSOCIATE ( As2 => As%Base )
      IF ( As2%GetID() .NE. 1 ) STOP 53
    END ASSOCIATE
  END ASSOCIATE


  END
