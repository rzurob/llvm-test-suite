! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  C808Misc2.f  
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
!*  TEST CASE NAME             : C808Misc2
!*  TEST CASE TITLE            : C808
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Oct. 26, 2004
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
!*    The selector is a constant object of derived type  
!*    (Comp failed-syntax err)   
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 
  MODULE M

    TYPE :: Base
      INTEGER  :: Id = 1
    CONTAINS
      PROCEDURE, PASS :: GetId => getbaseid
    END TYPE

  CONTAINS

    SUBROUTINE printbase()
      PRINT *,'base'
    END SUBROUTINE

    FUNCTION getbaseid(A)
      CLASS(Base), INTENT(IN) :: A
      INTEGER :: getbaseid
      getbaseid = a%id
    END FUNCTION

  END MODULE

  PROGRAM C808Misc2
  USE M
  IMPLICIT NONE

    TYPE(Base), PARAMETER :: V = Base(1)
    
    ASSOCIATE ( As => V)
      IF ( As%GetId() .NE. 1 ) STOP 11
    END ASSOCIATE

  END
