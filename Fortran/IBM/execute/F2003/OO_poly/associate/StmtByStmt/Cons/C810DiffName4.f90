! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qfree=f90
! %GROUP: C810DiffName4.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : C810DiffName4
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
!*     - Missing construct names in nested associate construct
!*    (Pass Exec-E level error)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM C810DiffName4
  IMPLICIT NONE
  INTEGER :: V = 1

  Cn : ASSOCIATE ( As  => V )
         ASSOCIATE ( As  => V )
           V = V + 10
           IF ( As .NE. 11 ) STOP 50
         END ASSOCIATE Cn
         IF ( As .NE. 11 )  STOP 52
       END ASSOCIATE

  END

