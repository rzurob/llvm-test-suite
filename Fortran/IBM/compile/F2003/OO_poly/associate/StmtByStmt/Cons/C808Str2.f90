! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP:  redherring.f  
! %VERIFY:  
! %STDIN:
! %STDOUT: 
! %EXECARGS:
! %POSTCMD: tcomp C808Str2.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : C808Str2
!*  TEST CASE TITLE            : C808
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Oct. 20, 2004
!*  ORIGIN                     : AIX Compiler Development, IBM Software Solutions Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Associate
!*
!*  SECONDARY FUNCTIONS TESTED : Selector is a constant
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
!*    The selector is a structure constructor with abstract parent component 
!*    (Pass Exce) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890
 

  PROGRAM C808Arr2
  IMPLICIT NONE

    TYPE, ABSTRACT :: Base
      INTEGER  :: Id = 0
    END TYPE

    TYPE, EXTENDS(Base) :: Child
    END TYPE
  
    ASSOCIATE ( As => Child(Id = 1) )
      As%Base%Id = 5
    
      ASSOCIATE ( As => As%Base )
      END ASSOCIATE

    END ASSOCIATE


  END
