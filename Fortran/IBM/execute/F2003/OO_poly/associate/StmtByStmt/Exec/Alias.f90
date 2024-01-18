! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: Alias.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : Alias
!*
!*  DATE                       : Mar. 01, 2005
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
!*    The associate name is reused many times
!*   ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM Alias
  IMPLICIT NONE

  ASSOCIATE ( As => 1 )
  ASSOCIATE ( As => 2 )
  ASSOCIATE ( As => 3 )
  ASSOCIATE ( As => 4 )
  ASSOCIATE ( As => 5 )
  ASSOCIATE ( As => 6 )
  ASSOCIATE ( As => 7 )
  ASSOCIATE ( As => 8 )
  ASSOCIATE ( As => 9 )
  ASSOCIATE ( As => 0 )
    IF ( As .NE. 0 ) STOP 20
  END ASSOCIATE
    IF ( As .NE. 9 ) STOP 19
  END ASSOCIATE
    IF ( As .NE. 8 ) STOP 18
  END ASSOCIATE
    IF ( As .NE. 7 ) STOP 17
  END ASSOCIATE
    IF ( As .NE. 6 ) STOP 16
  END ASSOCIATE
    IF ( As .NE. 5 ) STOP 15
  END ASSOCIATE
    IF ( As .NE. 4 ) STOP 14
  END ASSOCIATE
    IF ( As .NE. 3 ) STOP 13
  END ASSOCIATE
    IF ( As .NE. 2 ) STOP 12
  END ASSOCIATE
    IF ( As .NE. 1 ) STOP 11
  END ASSOCIATE

  END

