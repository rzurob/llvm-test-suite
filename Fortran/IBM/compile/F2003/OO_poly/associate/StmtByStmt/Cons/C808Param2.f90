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
! %POSTCMD: tcomp C808Param2.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : C808Param2
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
!*    The selector is a string constant
!*    (ICE) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM C808Param2

    CHARACTER(10) :: Hello
    PARAMETER (Hello="Hello")

    print*, Hello 
    ASSOCIATE ( As => Hello )
      As = "changed!" 
      print*, As 
    END ASSOCIATE

  END
