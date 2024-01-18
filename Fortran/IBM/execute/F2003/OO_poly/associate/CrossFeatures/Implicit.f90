! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  Implicit.f
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
!*  TEST CASE NAME             : Implicit 
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
!*    The selector is an entity with implicit type 
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
 
  PROGRAM Implicit 

  USE M
  IMPLICIT TYPE(DT)(A) 

  ASSOCIATE( As => As )

    IF ( As%ID      .NE. 1 ) STOP 30
    IF ( As%GetID() .NE. 1 ) STOP 31

    As%ID = 2
 
    IF ( As%ID      .NE. 2 ) STOP 40
    IF ( As%GetID() .NE. 2 ) STOP 41

  END ASSOCIATE

  IF ( As%ID      .NE. 2 ) STOP 50
  IF ( As%GetID() .NE. 2 ) STOP 51

  END


