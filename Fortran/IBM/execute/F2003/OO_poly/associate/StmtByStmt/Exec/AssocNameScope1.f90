! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: AssocNameScope1.f 
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
!*  TEST CASE NAME             : AssocNameScope.f 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Mar. 04, 2005
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
!*    
!*  The associate name's scope 
!*   
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890



  PROGRAM AssocNameScopei1 
  IMPLICIT NONE
  CLASS(*), ALLOCATABLE :: V

  ALLOCATE(V, SOURCE=1_1)

  ASSOCIATE ( A => V)
    ASSOCIATE ( A => A )

      SELECT TYPE ( A )
      CLASS DEFAULT
        STOP 10
      TYPE IS ( INTEGER(1) )
        A = 2_1
      END SELECT

      SELECT TYPE ( V )
      CLASS DEFAULT
        STOP 10
      TYPE IS ( INTEGER(1) )
        IF ( V .NE. 2_1 ) STOP 11 
        V = 3_1
      END SELECT
 
    END ASSOCIATE

    SELECT TYPE ( A )
    CLASS DEFAULT
      STOP 10
    TYPE IS ( INTEGER(1) )
      IF ( A .NE. 3_1 ) STOP 12 
      A = 4_1
    END SELECT

  END ASSOCIATE

  SELECT TYPE ( V )
  CLASS DEFAULT
    STOP 10
  TYPE IS ( INTEGER(1) )
    IF ( V .NE. 4_1 ) STOP 13 
  END SELECT
   

  END
