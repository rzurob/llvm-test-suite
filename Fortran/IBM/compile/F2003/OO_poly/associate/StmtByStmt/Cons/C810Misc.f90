! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: redherring.f 
! %VERIFY:  
! %STDIN:
! %STDOUT:  
! %EXECARGS:
! %POSTCMD: tcomp C810Misc.f 
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : C810Misc
!*  TEST CASE TITLE            : C809
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Oct. 20, 2004
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
!*     C810 : Miscellaneous problems found  
!*     - IMPLICIT does not work within associate construct 
!*    (Pass exec) : V, As, W- none of them are correct usage
!*
!234567890123456789012345678901234567890123456789012345678901234567890
 

  PROGRAM C810Misc
  IMPLICIT NONE
  !INTEGER :: V = 1

  ASSOCIATE ( As  => V )
      V = 10
      As = V 
      W = 11
      PRINT*, V 
      PRINT*, As 
      PRINT*, W 
  END ASSOCIATE 

  END

