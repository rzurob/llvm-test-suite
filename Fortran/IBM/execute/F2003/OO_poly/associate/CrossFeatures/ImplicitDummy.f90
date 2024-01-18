! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  ImplicitDummy.f
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
!*  TEST CASE NAME             : ImplicitDummy 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 09, 2005
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
!*    The selector is a dummy entity with implicit type 
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 
  MODULE M
    TYPE :: DT
      INTEGER :: Id = 1
      CONTAINS
      PROCEDURE, PASS   :: GetId
    END TYPE

  CONTAINS

    ELEMENTAL FUNCTION GetId(Arg)
    CLASS(DT), INTENT(IN) :: Arg
    INTEGER               :: GetId
      GetId = Arg%Id
    END FUNCTION

  END MODULE
 
  PROGRAM ImplicitDummy 

  USE M
  IMPLICIT TYPE(DT)(A-B) 

  CALL Sub(A)
  IF ( A%ID      .NE. 2 ) STOP 60
  IF ( A%GetID() .NE. 2 ) STOP 61

  CONTAINS

  SUBROUTINE Sub(B)

  ASSOCIATE( As => B )

    IF ( As%ID      .NE. 1 ) STOP 30
    IF ( As%GetID() .NE. 1 ) STOP 31

    As%ID = 2
 
    IF ( As%ID      .NE. 2 ) STOP 40
    IF ( As%GetID() .NE. 2 ) STOP 41

  END ASSOCIATE

  IF ( B%ID      .NE. 2 ) STOP 50
  IF ( B%GetID() .NE. 2 ) STOP 51

  END SUBROUTINE

  END


