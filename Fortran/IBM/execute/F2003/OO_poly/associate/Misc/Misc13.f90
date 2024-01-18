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
!*  Associating entity keeps bounds of selector
!* (Exec wrong )
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM Misc13

  CALL Sub((/(i, i=1,4)/) )

  CONTAINS

  SUBROUTINE Sub(Arg)
   CLASS(*):: Arg(3:)

   SELECT TYPE (Arg)
   TYPE IS (Integer)
     IF( ANY(LBOUND(Arg) .NE. (/3/)) ) ERROR STOP 30
     ASSOCIATE ( As => Arg )
       IF( ANY(LBOUND(As) .NE. (/3/)) ) ERROR STOP 31
     END ASSOCIATE
   END SELECT

  END SUBROUTINE

  END

