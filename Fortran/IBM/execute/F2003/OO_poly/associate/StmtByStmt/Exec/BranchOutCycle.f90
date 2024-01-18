! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  BranchOutCycle.f  
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
!*  TEST CASE NAME             : BranchOutCycle
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
!*  Branch out of an associate construct with CYCLE 
!* (Comp failed : " Construct name on CYCLE does not match name on DO-loop." )
!*  
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM BranchOutCycle 
  IMPLICIT NONE
 
  INTEGER :: i

  ASSOCIATE (As0  => i )
    D: DO i = 1, 5
      ASSOCIATE (As1  => As0 )
        CYCLE D 
      END ASSOCIATE
    END DO D
    PRINT*, "OK!"
  END ASSOCIATE

  END
