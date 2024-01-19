! *********************************************************************
!*  ===================================================================
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
!*    The selector is an associte name associating to a constant expression
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM HostAssocConstExp
  IMPLICIT NONE

  INTEGER :: i
  INTEGER :: Int1(10) = 1

    ASSOCIATE ( T0 => Int1 + (/( i, i = 1, 10) /) )
    ASSOCIATE ( As0 => T0)
    ASSOCIATE ( As1 => As0(1:10:2))
      IF ( Any( As0 .NE. (/( i+1, i = 1, 10) /)) )    ERROR STOP 40
      IF ( Any( As1 .NE. (/( i+1, i = 1, 10, 2) /)) ) ERROR STOP 41
    END ASSOCIATE
    END ASSOCIATE
    END ASSOCIATE

  END
