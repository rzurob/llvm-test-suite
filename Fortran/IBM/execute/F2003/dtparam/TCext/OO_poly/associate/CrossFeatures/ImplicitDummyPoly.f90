! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qreuse=all -qdeferredlp /tstdev/OO_poly/associate/CrossFeatures/ImplicitDummyPoly.f
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  ImplicitDummyPoly.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : ImplicitDummyPoly
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
!*    The selector is a dummy entity with implicit type
!*    ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  MODULE M
    TYPE :: DT(K1)    ! (4)
      INTEGER, KIND :: K1
      INTEGER(K1)   :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    IMPLICIT CLASS(DT(4))(A,B)
    INTENT(IN) :: Arg
    INTEGER    :: GetId
      GetId = Arg%Id
    END FUNCTION

  END MODULE

  PROGRAM ImplicitDummy

  USE M
  IMPLICIT TYPE(DT(4))(A), CLASS(*)(B)

  CALL Sub(A)
  IF ( A%ID      .NE. 2 ) STOP 60
  IF ( A%GetID() .NE. 2 ) STOP 61

  CONTAINS

  SUBROUTINE Sub(B)

  ASSOCIATE( As => B )
  SELECT TYPE( As )
  CLASS IS (DT(4))

    IF ( As%ID      .NE. 1 ) STOP 30
    IF ( As%GetID() .NE. 1 ) STOP 31

    As%ID = 2

    IF ( As%ID      .NE. 2 ) STOP 40
    IF ( As%GetID() .NE. 2 ) STOP 41

  CLASS DEFAULT
    STOP 99
  END SELECT
  END ASSOCIATE

  END SUBROUTINE

  END


