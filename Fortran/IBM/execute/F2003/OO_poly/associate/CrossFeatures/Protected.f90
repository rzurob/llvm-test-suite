! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  Protected.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Protected
!*
!*  DATE                       : Mar. 09, 2005
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
!*    The selector is a protected entity
!*    (comp failed-300949)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT
      INTEGER :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

    CLASS(*), POINTER, PROTECTED :: T

  CONTAINS

    FUNCTION GetId(Arg)
    CLASS(DT), INTENT(IN) :: Arg
    INTEGER               :: GetId
      GetId = Arg%Id
    END FUNCTION

    SUBROUTINE Set(Arg)
    CLASS(*), INTENT(IN) :: Arg
      ASSOCIATE( AS => Arg)
        ALLOCATE(T, SOURCE=As)
      END ASSOCIATE
    END SUBROUTINE

  END MODULE

  PROGRAM Protected

  USE M
  IMPLICIT NONE

  CALL Set(DT(ID=-1))
  ASSOCIATE( As => T )

    SELECT TYPE(As)
    CLASS IS (DT)

      IF ( As%ID      .NE. -1 ) STOP 30
      IF ( As%GetID() .NE. -1 ) STOP 31

      CALL Set(DT(ID=2))

      SELECT TYPE(T)
      CLASS IS (DT)

        IF ( T%ID      .NE. 2 ) STOP 40
        IF ( T%GetID() .NE. 2 ) STOP 41

      CLASS DEFAULT
        STOP 98
      END SELECT

    CLASS DEFAULT
      STOP 99
    END SELECT

  END ASSOCIATE

  END


