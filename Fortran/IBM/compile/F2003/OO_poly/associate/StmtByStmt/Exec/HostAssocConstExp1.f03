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
!*    This is a diagnostic test on associate name appearing in variable definition context.
!*    (pass exec)
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM HostAssocConstExp1
  IMPLICIT NONE

  INTEGER :: i
  INTEGER :: Int1(10) = 2

    ASSOCIATE ( T0 => Int1 + (/( i, i = 1, 10) /) )
    ASSOCIATE ( As0 => T0)
       As0 =1
       print*, As0
    END ASSOCIATE
    END ASSOCIATE

  END
