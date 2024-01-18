! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP:  BranchOut1.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : BranchOut1
!*
!*  DATE                       : Nov. 02, 2004
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
!*  Branch out of an associate construct and the target is still in another
!*  associate construct
!*  (ICE: signal 11 received)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM BranchOut
  IMPLICIT NONE

  ASSOCIATE (As0  => 1 )
    ASSOCIATE (As1  => As0 )
      GOTO 10
    END ASSOCIATE

    10 PRINT*, "OK!"
  END ASSOCIATE


  END
