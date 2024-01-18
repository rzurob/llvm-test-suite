! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/VarDummy.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  VarDummy.f  
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
!*  TEST CASE NAME             : VarDummy
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
!*    The selector is a non poly pointer dummy variable of derived types
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

  PROGRAM VarDummy
  USE M
  IMPLICIT NONE
 
  TYPE(Child(4)), TARGET :: U
  TYPE(Child(4)), POINTER     :: Ptr
  TYPE(Child(4)), POINTER     :: Ptr0  
 
  Ptr => U
  Ptr0 => Ptr
  CALL Sub(Ptr, Ptr0)
 
  CONTAINS

  SUBROUTINE Sub(Ptr0, Ptr1)
  IMPLICIT NONE
  TYPE(Child(4)), POINTER :: Ptr0, Ptr1  
 
  ALLOCATE(Ptr1%BaseComp)

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

  ASSOCIATE (As =>  Ptr1%GetID())
    IF ( As .NE. 2 ) STOP 60
  END ASSOCIATE

  ASSOCIATE (As =>  Ptr0%Base%GetID())
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
  
  END SUBROUTINE

  END
