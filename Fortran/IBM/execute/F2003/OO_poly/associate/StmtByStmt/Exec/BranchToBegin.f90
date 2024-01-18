! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  BranchToBegin.f  
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
!*  TEST CASE NAME             : BranchToBegin
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 25, 2005
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
!*  Branch to the begin of associate statement 
!*  () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM BranchToBegin
  IMPLICIT NONE

  CALL Sub("ok")
  CONTAINS
  SUBROUTINE Sub(Arg)
  CLASS(*) :: Arg
 
  GOTO 1
1 ASSOCIATE:ASSOCIATE ( ASSOCIATE => Arg )
    SELECT TYPE ( ASSOCIATE)
    TYPE IS (CHARACTER(*))
      IF ( ASSOCIATE .NE. "ok" ) STOP 20 
      GOTO 2
    END SELECT
2 END ASSOCIATE ASSOCIATE
  END SUBROUTINE

  END
