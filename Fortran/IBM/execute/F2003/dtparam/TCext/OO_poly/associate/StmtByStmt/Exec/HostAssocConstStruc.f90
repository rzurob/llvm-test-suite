! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/StmtByStmt/Exec/HostAssocConstStruc.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ConstStruct.f  
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
!*  TEST CASE NAME             : ConstStruct 
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
!*    The selector is a constant structure (component) 
!*   (Coredump)   
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
    CONTAINS
      PROCEDURE, NOPASS :: PrintType => PrintChild 
      PROCEDURE, PASS   :: GetId => GetChildId 
    END TYPE

    CONTAINS

    SUBROUTINE PrintChild()
      PRINT *,'Child'
    END SUBROUTINE

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

  PROGRAM ConstStruct 
  USE M
  IMPLICIT NONE

  TYPE(Child(4)) :: V = Child(4)() 
  TYPE(Child(4)), PARAMETER :: W = Child(4)() 
  
    ASSOCIATE ( As => Child(4)() )
      IF ( As%GetID() .NE. 2) STOP 50 
      ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
         IF ( As0 .NE. 2) STOP 51 
         IF ( As1 .NE. 1) STOP 52 
      END ASSOCIATE

      ASSOCIATE ( As2 => As%Base )
        IF ( As2%BaseID .NE. 1 ) STOP 53
      END ASSOCIATE
    END ASSOCIATE

    ASSOCIATE ( As => V )
      IF ( As%GetID() .NE. 2) STOP 54 
      ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
         IF ( As0 .NE. 2) STOP 55 
         IF ( As1 .NE. 1) STOP 56 
      END ASSOCIATE

      ASSOCIATE ( As => As%Base )
        IF ( As%GetID() .NE. 1 ) STOP 57
      END ASSOCIATE

      IF( As%GetID() .NE. 2 ) STOP 57
    END ASSOCIATE

    ASSOCIATE ( As => W )
      IF ( As%GetID() .NE. 2) STOP 58 
      ASSOCIATE ( As0 => As%ChildId, As1 => As%BaseId )
         IF ( As0 .NE. 2) STOP 59 
         IF ( As1 .NE. 1) STOP 60 
      END ASSOCIATE

      ASSOCIATE ( As => As%Base )
        IF ( As%GetId() .NE. 1 ) STOP 61 
      END ASSOCIATE

      ASSOCIATE ( As => W%Base%GetID() )
        IF ( As .NE. 1 ) STOP 62 
      END ASSOCIATE
    END ASSOCIATE

  END
