! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/HostAssocVarPolyPtr.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  HostAssocVarPolyPtr.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : HostAssocVarPolyPtr
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
!*    The selector is an associate name associating to a poly pointer variable of derived types
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND           :: K1
      INTEGER(K1)             :: BaseId = 1
      TYPE(Base(K1)), POINTER :: BasePtr => NULL()
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1)  :: ChildId = 2
      TYPE(Child(K1)), POINTER :: ChildPtr => NULL()
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

  PROGRAM HostAssocVarPolyPtr
  USE M
  IMPLICIT NONE

  TYPE(Base(4)),  TARGET      :: V
  TYPE(Child(4)), TARGET      :: U
  TYPE(Base(4)),  POINTER    ::  BasePtr
  TYPE(Child(4)), POINTER     :: ChildPtr

  BasePtr => V
  ChildPtr => U

  ASSOCIATE ( T1  => BasePtr, T2 => ChildPtr, T3 => V, T4 => U  )
  ASSOCIATE ( As1 => T1, As2 => T2  )
    IF ( As1%GetID() .NE. 1) STOP 50
    IF ( As2%GetID() .NE. 2) STOP 51

    T1%BasePtr  => T3
    T2%ChildPtr => T4

    IF ( .NOT. ASSOCIATED( As1%BasePtr, V) )  STOP 60
    IF ( .NOT. ASSOCIATED( As2%ChildPtr, U) ) STOP 61

    U%BaseId  = -1
    U%ChildId = -2

    IF ( As2%Base%GetID() .NE. -1) STOP 70
    IF ( As2%GetID()      .NE. -2) STOP 71

  END ASSOCIATE
  END ASSOCIATE


  END
