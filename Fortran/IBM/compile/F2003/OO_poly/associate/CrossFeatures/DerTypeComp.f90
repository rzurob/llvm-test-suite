! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  redherring.f
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD:  tcomp DerTypeComp.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : DerTypeComp
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 07, 2005
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
!*    Illegal usage on componet:
!*    Abstract/private componet as selector   
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M0

    TYPE, ABSTRACT :: Base
      INTEGER :: BaseId = 1
    END TYPE

  END MODULE 

  MODULE M1
  USE M0, DT0=>Base

    TYPE, EXTENDS(DT0) :: Child
      PRIVATE
      INTEGER  :: ChildId = 2
    END TYPE

  END MODULE

  MODULE M
  USE M1, DT=>Child

  TYPE(DT), SAVE :: T

  END MODULE
  
  PROGRAM DerTypeSeq
  USE M, V=>T
  IMPLICIT NONE


  ASSOCIATE( As => V )
    ASSOCIATE( As => As%Base )
    END ASSOCIATE

    ASSOCIATE( As => As%ChildID )
    END ASSOCIATE
  END ASSOCIATE


  END


