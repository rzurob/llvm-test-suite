!*********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS:  -qfree=f90 
! %GROUP:   C808Param1.f 
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
!*  TEST CASE NAME             : C808Param1
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
!*    The selector is a string literal 
!*    (Seg fault) 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  PROGRAM C808Param1

   ASSOCIATE ( As => "try!" )
     PRINT*, As
     IF ( As .NE. "try!" ) STOP 10
   END ASSOCIATE
   PRINT*, 'OK!'

  END
