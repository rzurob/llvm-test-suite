! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/HostAssocVarDer.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  HostAssocVarDer.f  
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
!*  TEST CASE NAME             : HostAssocVarDer 
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
!*    The selector is an associate name associating to a  non poly variable of derived types
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
      TYPE(Base(K1)) :: BaseArr(3) 
    CONTAINS
      PROCEDURE, PASS   :: GetId => GetChildId 
    END TYPE

    TYPE(Child(4))  :: V = Child(4)(BaseArr=(/Base(4)(), Base(4)(), Base(4)() /)) 

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

  PROGRAM HostAssocVarDer 
  USE M, U => V
  IMPLICIT NONE

  ASSOCIATE (As0 =>  U%GetID())
  ASSOCIATE (As  =>  As0)
    IF ( As .NE. 2 ) STOP 60
  END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE (As =>  U%Base%GetID())
  ASSOCIATE (As =>  As)
    IF ( As .NE. 1 ) STOP 61
  END ASSOCIATE
  END ASSOCIATE

  ASSOCIATE ( V => U )
  ASSOCIATE ( As => V )
    IF ( ASSOCIATED(As%BasePtr)) STOP 49 
    IF ( As%GetID() .NE. 2) STOP 50 
    ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
       IF ( As0 .NE. 2) STOP 51 
       IF ( As1 .NE. 1) STOP 52 
    END ASSOCIATE
    ASSOCIATE ( As2 => As%Base )
      IF ( As2%GetID() .NE. 1 ) STOP 53
    END ASSOCIATE

    U%BaseId = -1
    U%ChildId = -2
    IF ( As%BaseId .NE. -1)  STOP 71 
    IF ( As%ChildId .NE. -2) STOP 72 
    
  END ASSOCIATE
  END ASSOCIATE

  END
