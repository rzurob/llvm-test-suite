! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: AssocNameAsOther5.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : AssocNameAsOther5
!*
!*  DATE                       : Feb. 28, 2005
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
!*    The associate construct name is the same as a specific interface name
!*   (300548-conflicting entity)
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  TYPE :: DT
    CHARACTER(8) :: C="12345678"
  END TYPE

  INTERFACE
    FUNCTION Fun(Arg)
    IMPORT DT
    CLASS(DT) :: Arg
    TYPE(DT) :: Fun
    END FUNCTION
  END INTERFACE

  INTERFACE
    FUNCTION F(Arg)
    INTEGER :: Arg, F
    END FUNCTION
  END INTERFACE

  END MODULE


  PROGRAM AssocNameAsOther5
  USE M

  ASSOCIATE ( Fun => Fun(DT("87654321")) )
    IF ( Fun%C .NE. "87654321" ) STOP 11
     ASSOCIATE ( F => Fun%C )
       IF ( F .NE. "87654321" ) STOP 12
     END ASSOCIATE
     IF ( F(1) .NE. 1 ) STOP 13
  END ASSOCIATE


  END

  FUNCTION F(Arg)
  INTEGER :: Arg, F
    F = Arg
  END FUNCTION

  FUNCTION Fun(Arg)
  USE M, ONLY:DT
  CLASS(DT) :: Arg
  TYPE(DT) :: Fun
    Fun = Arg
  END FUNCTION

