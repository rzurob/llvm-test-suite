! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: 
! %COMPOPTS: -qfree=f90 
! %GROUP: AssocNameAsOther1.f 
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
!*  TEST CASE NAME             : AssocNameAsOther1 
!*  TEST CASE TITLE            : 
!*
!*  PROGRAMMER                 : Feng Ye
!*  DATE                       : Feb. 28, 2005
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
!*    The associate construct name is the same as intrisics names 
!*    
!*
!234567890123456789012345678901234567890123456789012345678901234567890


  PROGRAM AssocNameAsOther1 
  INTEGER :: i
  REAL    :: R
  DOUBLE PRECISION :: DP
  CHARACTER :: C
  BYTE    :: B
  LOGICAL :: L

  ASSOCIATE ( Integer => 1  )
    IF ( Integer .NE. 1 ) Stop 11
  END ASSOCIATE 

  ASSOCIATE ( Real => -1.0  )
    IF ( Real .NE. -1.0 ) Stop 12
  END ASSOCIATE 

  ASSOCIATE ( Double => 2.0D0  )
    IF ( Double .NE. 2.0D0 ) Stop 13
  END ASSOCIATE 

  ASSOCIATE ( Character => "!!!"  )
    IF ( Character(:) .NE. "!!!" ) Stop 14
  END ASSOCIATE 

  ASSOCIATE ( Byte => 1  )
    IF ( Byte .NE. 1 ) Stop 15
  END ASSOCIATE 

  ASSOCIATE ( Logical => .FALSE.  )
    IF ( Logical .NEQV. .FALSE. ) Stop 16
  END ASSOCIATE 

  END

