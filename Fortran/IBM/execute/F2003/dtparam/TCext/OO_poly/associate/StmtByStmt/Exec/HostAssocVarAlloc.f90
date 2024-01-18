! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/HostAssocVarAlloc.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  HostAssocVarAlloc.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:  
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : HostAssocVarAllo 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Nov. 02, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  REFERENCE                  : Feature 219934
!*
!*  DRIVER STANZA              :
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION
!*    The selector is an associate name associating to a nonpoly allocatable variable of derived types
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M

    TYPE :: Base(K1)    ! (4)
      INTEGER, KIND            :: K1
      INTEGER(K1), ALLOCATABLE :: BaseId 
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetBaseId
    END TYPE

    TYPE, EXTENDS(Base) :: Child    ! (4)
      INTEGER(K1), ALLOCATABLE  :: ChildId
      TYPE(Base(K1))            :: BaseComp 
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId 
    END TYPE

    CONTAINS

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

  PROGRAM HostAssocVarAlloc 
  USE M
  IMPLICIT NONE

  TYPE(Child(4)), ALLOCATABLE   :: U  
 
  ALLOCATE(U)

  ASSOCIATE ( As => U )
  ASSOCIATE ( As => As )
    ALLOCATE(As%ChildId, SOURCE=2)
    ALLOCATE(As%BASE%BaseId, SOURCE=1)

    IF ( As%GetID() .NE. 2) STOP 50 
    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( As0 .NE. 2) STOP 51 
       IF ( As1 .NE. 1) STOP 52 
    END ASSOCIATE
    ASSOCIATE ( As2 => As%Base )
      IF ( As2%GetID() .NE. 1 ) STOP 53
    END ASSOCIATE

    As%ChildId = -2
    As%BaseId  = -1
    IF (U%GetID()       .NE. -2 ) STOP 61
    IF (U%Base%GetID()  .NE. -1 ) STOP 62

  END ASSOCIATE
  END ASSOCIATE


  END
