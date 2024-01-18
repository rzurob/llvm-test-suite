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
! %POSTCMD: tcomp VarDef7.f
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : VarDef7
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb 22, 2005
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
!*   Variable Definition Context on non variable selector 
!*   - Write 
!*    () 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

  MODULE M
  CHARACTER(9), PROTECTED :: C
  END MODULE

  PROGRAM VarDef7
  USE M
  IMPLICIT NONE

  CALL Sub("1234567890")
  CONTAINS

  SUBROUTINE Sub(Arg)
  CHARACTER(*), INTENT(IN) :: Arg 

  ASSOCIATE ( As => C )
   WRITE(As,*) 1 
  END ASSOCIATE

  ASSOCIATE ( As => Arg )
     WRITE(As,*) 1 
  END ASSOCIATE

  END SUBROUTINE
  END 

