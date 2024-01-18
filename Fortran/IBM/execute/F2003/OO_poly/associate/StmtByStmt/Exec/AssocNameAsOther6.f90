! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: AssocNameAsOther6.f 
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
!*  TEST CASE NAME             : AssocNameAsOther6 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 01, 2005
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
!*    The associate construct name is the same as a program unit name 
!*   () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

 

  PROGRAM AssocNameAsOther6
 
  ASSOCIATE ( AssocNameAsOther6 => (/("ok", i=1,127)/))
    IF ( ANY(SHAPE(AssocNameAsOther6) .NE. (/127/) )) STOP 11 
    IF ( ANY(AssocNameAsOther6    .NE. "ok") )        STOP 12 

     ASSOCIATE ( AssocNameAsOther6 => F(123))
       IF ( AssocNameAsOther6 .NE. 123 )   STOP 13 
     END ASSOCIATE 

     ASSOCIATE ( F => AssocNameAsOther6 )
       IF ( ANY(SHAPE(F) .NE. (/127/) ))    STOP 21 
       IF ( ANY(F        .NE. "ok") )       STOP 22 
     END ASSOCIATE 

  END ASSOCIATE 

  CONTAINS

  FUNCTION F(Arg)
  INTEGER :: Arg, F
    F = Arg
  END FUNCTION

  END


