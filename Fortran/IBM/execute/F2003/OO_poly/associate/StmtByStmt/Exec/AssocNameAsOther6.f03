! *********************************************************************
!*  ===================================================================
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
!*    The associate construct name is the same as a program unit name
!*   ()
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM AssocNameAsOther6

  ASSOCIATE ( AssocNameAsOther6 => (/("ok", i=1,127)/))
    IF ( ANY(SHAPE(AssocNameAsOther6) .NE. (/127/) )) ERROR STOP 11
    IF ( ANY(AssocNameAsOther6    .NE. "ok") )        ERROR STOP 12

     ASSOCIATE ( AssocNameAsOther6 => F(123))
       IF ( AssocNameAsOther6 .NE. 123 )   ERROR STOP 13
     END ASSOCIATE

     ASSOCIATE ( F => AssocNameAsOther6 )
       IF ( ANY(SHAPE(F) .NE. (/127/) ))    ERROR STOP 21
       IF ( ANY(F        .NE. "ok") )       ERROR STOP 22
     END ASSOCIATE

  END ASSOCIATE

  CONTAINS

  FUNCTION F(Arg)
  INTEGER :: Arg, F
    F = Arg
  END FUNCTION

  END

