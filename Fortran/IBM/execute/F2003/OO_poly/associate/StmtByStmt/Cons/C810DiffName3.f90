! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: C810DiffName3.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C810DiffName3
!*
!*  DATE                       : Oct. 20, 2004
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Associate construct name
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
!*     C810 : The associate construct shall have the same name
!*     - Usage of names
!*   (Seg fault-301466)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C810DiffName3
  IMPLICIT NONE

     As1 : ASSOCIATE ( As1  => 11 )
       As:   ASSOCIATE ( As  => as1 )
         IF ( As .NE. 11 ) STOP 50
       END ASSOCIATE As
       IF ( As1 .NE. 11 ) STOP 51
     END ASSOCIATE As1

  END

